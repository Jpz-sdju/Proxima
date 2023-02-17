/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package XiaoHe.SSDfrontend

import XiaoHe.SSDbackend.{SSDCoreConfig, myDebug}
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import top.Settings
import _root_.utils._
import XiaoHe._
import XiaoHe.SSDbackend._
import XiaoHe.SSDbackend.fu.ALUOpType
import XiaoHe.SSDfrontend._
class TableAddr(val idxBits: Int) extends NutCoreBundle {
  val padLen = if (Settings.get("IsRV32") || !Settings.get("EnableOutOfOrderExec")) 2 else 3
  def tagBits = VAddrBits - padLen - idxBits

  //val res = UInt((AddrBits - VAddrBits).W)
  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val pad = UInt(padLen.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
}

object BTBtype {
  def B = "b01".U  // branch
  def J = "b00".U  // jump
  def C = "b10".U  // call
  def R = "b11".U  // return

  def apply() = UInt(2.W)
}

class BPUUpdateReq extends NutCoreBundle {
  val valid = Output(Bool())
  val pc = Output(UInt(VAddrBits.W))
  val isMissPredict = Output(Bool())
  val actualTarget = Output(UInt(VAddrBits.W))
  val actualTaken = Output(Bool())  // for branch
  val fuOpType = Output(FuOpType())
  val btbType = Output(BTBtype())
  val isRVC = Output(Bool()) // for ras, save PC+2 to stack if is RVC
  val ghrNotUpdated = Output(UInt(GhrLength.W))
  val btbBtypeMiss = Output(Bool())
}

// nextline predicter generates NPC from current NPC in 1 cycle
class BPU_ooo extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Flipped(Valid((UInt(VAddrBits.W))))
      val ghr = Input(UInt(GhrLength.W))
    }
    val out = new RedirectIO
    val flush = Input(Bool())
    val brIdx = Output(Vec(4, Bool()))
    val crosslineJump = Output(Bool())
  })

  val flush = BoolStopWatch(io.flush, io.in.pc.valid, startHighPriority = true)

  //get pht index
  def getPhtIndex(pc:UInt, ghr:UInt) = {
    //val phtIndex = Cat(ghr(4,0) ^ Cat(ghr(8,7),0.U(3.W)).asUInt, pc(6,5) ^ ghr(6,5), pc(4,3))//88.198%
    val phtIndex = pc(9,3)
    phtIndex
  }

  def outputHold(out: Data, validLatch: Bool) = {
    val outLatch = RegEnable(out, 0.U.asTypeOf(out), validLatch)
    val output = Mux(validLatch,out,outLatch)
    output
  }
  val validLatch = RegNext(io.in.pc.valid)

  def genInstValid(pc: UInt) = LookupTree(pc(2,1), List(
    "b00".U -> "b1111".U,
    "b01".U -> "b1110".U,
    "b10".U -> "b1100".U,
    "b11".U -> "b1000".U
  ))

  // BTB
  val NRbtb = 512
  val NRbht = 2048
  val btbAddr = new TableAddr(log2Up(NRbtb >> 2))
  def btbEntry() = new Bundle {
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(VAddrBits.W)
    val crosslineJump = Bool()
    val valid = Bool()
  }

  val btb = List.fill(4)(Module(new SRAMTemplate(btbEntry(), set = NRbtb >> 2, shouldReset = true, holdRead = true, singlePort = true)))
  // flush BTB when executing fence.i
  val flushBTB = WireInit(false.B)
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushBTB, "MOUFlushICache")
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  (0 to 3).map(i => (btb(i).reset := reset.asBool || (flushBTB || flushTLB)))

  Debug(reset.asBool || (flushBTB || flushTLB), "[BPU-RESET] bpu-reset flushBTB:%d flushTLB:%d\n", flushBTB, flushTLB)

  (0 to 3).map(i => (btb(i).io.r.req.valid := io.in.pc.valid))
  (0 to 3).map(i => (btb(i).io.r.req.bits.setIdx := btbAddr.getIdx(io.in.pc.bits)))


  val btbRead = Wire(Vec(4, btbEntry()))
  (0 to 3).map(i => (btbRead(i) := btb(i).io.r.resp.data(0)))
  // since there is one cycle latency to read SyncReadMem,
  // we should latch the input pc for one cycle
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbHit = Wire(Vec(4, Bool()))
  (0 to 3).map(i => btbHit(i) := btbRead(i).valid && btbRead(i).tag === btbAddr.getTag(pcLatch) && !flush && RegNext(btb(i).io.r.req.fire(), init = false.B))
  // btbHit will ignore pc(2,0). pc(2,0) is used to build brIdx
  val brIdx = VecInit(Seq.fill(4)(false.B))
  val crosslineJump = btbRead(3).crosslineJump && btbHit(3) && !brIdx(0) && !brIdx(1) && !brIdx(2)
  io.crosslineJump := crosslineJump
  val pcLatchValid = genInstValid(pcLatch)
  val btbIsBranch = Wire(Vec(4, Bool()))
  (0 to 3).map(i => (btbIsBranch(i) := btbRead(i).valid && (btbRead(i)._type === BTBtype.B) && pcLatchValid(i).asBool && btbRead(i).tag === btbAddr.getTag(pcLatch)))
  io.out.btbIsBranch := outputHold(btbIsBranch.asUInt(),validLatch)
  // PHT
//  val pht = List.fill(4)(Mem(NRbht >> 2, UInt(2.W)))
  val pht = List.fill(4)(RegInit(VecInit(Seq.fill(NRbht >> 2)((2.U(2.W))))))
  val phtTaken = Wire(Vec(4, Bool()))
  val phtindex = getPhtIndex(io.in.pc.bits,io.in.ghr)
  (0 to 3).map(i => (phtTaken(i) := RegEnable(pht(i)(phtindex)(1), io.in.pc.valid)))
//  dontTouch(phtTaken)

  // RAS
  val NRras = 16
  val ras = Mem(NRras, UInt(VAddrBits.W))
  val sp = Counter(NRras)
  val rasTarget = RegEnable(ras.read(sp.value), io.in.pc.valid)

  // update
  val req = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry()))
  BoringUtils.addSink(req, "bpuUpdateReq")
//  dontTouch(btbWrite)

  btbWrite.tag := btbAddr.getTag(req.pc)
  btbWrite.target := req.actualTarget
  btbWrite._type := req.btbType
  btbWrite.crosslineJump := req.pc(2,1)==="h3".U && !req.isRVC // ((pc_offset % 8) == 6) && inst is 32bit in length
  btbWrite.valid := true.B
  // NOTE: We only update BTB at a miss prediction.
  // If a miss prediction is found, the pipeline will be flushed
  // in the next cycle. Therefore it is safe to use single-port
  // SRAM to implement BTB, since write requests have higher priority
  // than read request. Again, since the pipeline will be flushed
  // in the next cycle, the read request will be useless.
  (0 to 3).map(i => btb(i).io.w.req.valid := (req.isMissPredict /*|| req.btbBtypeMiss*/) && req.valid && i.U === req.pc(2,1))
  (0 to 3).map(i => btb(i).io.w.req.bits.setIdx := btbAddr.getIdx(req.pc))
  (0 to 3).map(i => btb(i).io.w.req.bits.data := btbWrite)

  val reqLatch = RegEnable(req,req.valid)
  val phtReadIndex = getPhtIndex(req.pc,req.ghrNotUpdated)
  val phtWriteIndex = getPhtIndex(reqLatch.pc,reqLatch.ghrNotUpdated)

  val getpht = LookupTree(req.pc(2,1), List.tabulate(4)(i => (i.U -> pht(i)(phtReadIndex))))
  val cnt = RegEnable(getpht,req.valid)
  val taken = reqLatch.actualTaken
  val newCnt = Mux(taken, cnt + 1.U, cnt - 1.U)
  val wen = (taken && (cnt =/= "b11".U)) || (!taken && (cnt =/= "b00".U))
  when (reqLatch.valid && ALUOpType.isBranch(reqLatch.fuOpType) && wen) {
//      (0 to 3).map(i => when(i.U === reqLatch.pc(2,1)){pht(i).write(phtWriteIndex, newCnt)})
    (0 to 3).map(i => when(i.U === reqLatch.pc(2,1)){pht(i)(phtWriteIndex) := newCnt})
  }

  //RAS speculative update
  val brIdxOneHot = Mux(brIdx(0),"b0001".U,Mux(brIdx(1),"b0010".U,Mux(brIdx(2),"b0100".U,Mux(brIdx(3),"b1000".U,"b0000".U))))
  val retIdx = VecInit(Seq.fill(4)(false.B))
  val retPC = Mux1H(brIdxOneHot,Seq(pcLatch+4.U,pcLatch+6.U,pcLatch+8.U,pcLatch+10.U))
  (0 to 3).map(i => retIdx(i) := (btbRead(i)._type === BTBtype.C) && (brIdxOneHot(i)))
  val rasWen = retIdx.asUInt.orR()
  val rasEmpty = RegEnable(sp.value === 0.U, io.in.pc.valid)

  when (rasWen)  {
    ras.write(sp.value + 1.U, retPC)  //TODO: modify for RVC
    sp.value := sp.value + 1.U
  }.elsewhen (req.valid && req.fuOpType === ALUOpType.ret) {
      when(sp.value === 0.U) {
        // RAS empty, do nothing
      }
          sp.value := Mux(sp.value===0.U, 0.U, sp.value - 1.U)
    }




  val target = Wire(Vec(4, UInt(VAddrBits.W)))
  (0 to 3).map(i => target(i) := Mux(btbRead(i)._type === BTBtype.R, rasTarget, btbRead(i).target))
  (0 to 3).map(i => brIdx(i) := btbHit(i) && pcLatchValid(i).asBool && Mux(btbRead(i)._type === BTBtype.B, phtTaken(i), Mux(btbRead(i)._type === BTBtype.R, !rasEmpty, true.B)) && btbRead(i).valid)
  io.brIdx := outputHold(brIdx,validLatch)
  io.out.target := outputHold(PriorityMux(io.brIdx, target),validLatch)
  io.out.valid := outputHold(io.brIdx.asUInt.orR,validLatch)
  io.out.rtype := 0.U

  //GHR
  val ghrLatch = RegEnable(io.in.ghr,io.in.pc.valid)
  //note the speculatibe ghr when more than one branch inst in a instline
  //divide the instline into two parts according to the position of the taken branch inst
  
  //                         ||                   || <-  jump / branch taken inst
  //   -----------------------------------------------------------------------------------------
  //   ||        3           ||          2        ||           1        ||          0         ||  <- instline
  //   -----------------------------------------------------------------------------------------
  //   ||   behind part      ||                             front part                        ||
  //   -----------------------------------------------------------------------------------------

  val jump = io.brIdx.asUInt.orR
  val frontMask = Wire(UInt(4.W))
  frontMask := Mux(!jump,"b1111".U,PriorityMux(io.brIdx,Seq("b0001".U,"b0011".U,"b0111".U,"b1111".U))) // when no jump, it will be "b1111".U
  val frontBranchVec = Wire(UInt(4.W))
  frontBranchVec := (frontMask & btbIsBranch.asUInt).asUInt
  val frontBranchNum = Wire(UInt(3.W))
  val frontBranchNumTmp0 = Wire(UInt(2.W))
  val frontBranchNumTmp1 = Wire(UInt(2.W))
  frontBranchNumTmp0 := frontBranchVec(0) + frontBranchVec(1)
  frontBranchNumTmp1 := frontBranchVec(2) + frontBranchVec(3)
  frontBranchNum := frontBranchNumTmp0 + frontBranchNumTmp1
  val branchTakenJump = Mux(jump,PriorityMux(io.brIdx,Seq(btbIsBranch(0),btbIsBranch(1),btbIsBranch(2),btbIsBranch(3))),false.B)
  val ghrPadding = (0.U(1.W) << (frontBranchNum - 1.U)).asUInt | branchTakenJump
  val ghrUpdated = (ghrLatch << frontBranchNum).asUInt | ghrPadding
  val ghrUpdateValid = frontBranchVec.asUInt.orR
  io.out.ghr := outputHold(ghrUpdated,validLatch)
  io.out.ghrUpdateValid := outputHold(ghrUpdateValid,validLatch)
  io.out.pc := io.in.pc.bits //not used


  //BPU brancn inst update req debug
  val cond = req.btbType === BTBtype.B && req.valid
  if(SSDCoreConfig().EnableBPUupdateDebug) {
    myDebug(cond, "BPUUpdate at pc:%x, btbBtypeMiss:%b, isMissPredict:%b, taken:%b, ghr:%b, target:%x, phtIndex:%x, cnt:%b\n",
      req.pc, req.btbBtypeMiss, req.isMissPredict, req.actualTaken, req.ghrNotUpdated, req.actualTarget, phtReadIndex,getpht)
  }
}

class DummyPredicter extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle { val pc = Flipped(Valid((UInt(VAddrBits.W)))) }
    val out = new RedirectIO_nooo
    val valid = Output(Bool())
    val flush = Input(Bool())
    val ignore = Input(Bool())
    val brIdx = Output(Vec(4, Bool()))
  })
  // Note: when io.ignore, io.out.valid must be false.B for this pc
  // This limitation is for cross instline inst fetch logic
  io.valid := io.in.pc.valid // Predicter is returning a result
  io.out.valid := false.B // Need redirect
  io.out.target := DontCare // Redirect target
  io.out.rtype := DontCare // Predicter does not need to care about it
  io.brIdx := VecInit(Seq.fill(4)(false.B)) // Which inst triggers jump
}

