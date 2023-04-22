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
  def tagBits = VAddrBits - padLen - idxBits*3

  //val res = UInt((AddrBits - VAddrBits).W)
  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val pad = UInt(padLen.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx

  ///////////////////
  def hashBTBAddr(pcIn :UInt) = Cat(pcIn(12,11) ^ pcIn(10,9),pcIn(8,3))
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
  val ghrFetch = Output(UInt(GhrLength.W))
  val btbBtypeMiss = Output(Bool())
}

// nextline predicter generates NPC from current NPC in 1 cycle
class BPU_ooo extends NutCoreModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Flipped(Valid((UInt(VAddrBits.W))))
      // val ghr = Input(UInt(GhrLength.W))
    }
    val out = new RedirectIO
    val flush = Input(Bool())
    val brIdx = Output(Vec(4, Bool()))
    val crosslineJump = Output(Bool())

    ///////jpz addiation

    val saveTheFetch = Output(Bool())
    val saveAddr = Output(UInt(VAddrBits.W))

    val fghr = Output(UInt(GhrLength.W))
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

  def count(x:UInt) = {
    val output = Wire(UInt(4.W))
    output := Cat(0.U(3.W), x(0)) +
    Cat(0.U(3.W), x(1)) +
    Cat(0.U(3.W), x(2)) +
    Cat(0.U(3.W), x(3)) 
    output
  }
  def hashBhtAddr(pc:UInt, btbAddr:UInt, fghr:UInt) = {
    val bhtAddr = Wire(UInt(9.W))
    // val bhtAddr = Wire(UInt(5.W))

    // bhtAddr := Cat(Cat(fghr(4,3),fghr(0)) ^ Cat(btbAddr(2,0)), fghr(2,1) ^ Cat(btbAddr(3),0.U)) =>84.6
    // bhtAddr :=  Cat(Cat(fghr(3,2),btbAddr(3)) ^ Cat(btbAddr(2,0)), Cat(fghr(0),fghr(1)) ^ Cat(fghr(4),1.U)) 
    // bhtAddr := fghr(4,0)

    bhtAddr := Cat(fghr(4,0), pc(6,3))
    // bhtAddr := pc(10,3)

    bhtAddr
  }

  def genInstValid(pc: UInt) = LookupTree(pc(2,1), List(
    "b00".U -> "b1111".U,
    "b01".U -> "b1110".U,
    "b10".U -> "b1100".U,
    "b11".U -> "b1000".U
  ))

  def decode24(in:UInt) = {
    val output = Wire(UInt(4.W))
    output := 0.U(4.W)
    switch(in) {
        is("b00".U) { output := "b0001".U}
        is("b01".U) { output := "b0010".U}
        is("b10".U) { output := "b0100".U}
        is("b11".U) { output := "b1000".U}
      }
    output
  }
  // BTB
  val NRbtb = 512
  val NRbht = 2048
  val btbAddr = new TableAddr(log2Up(NRbtb >> 2))
  def btbEntry() = new Bundle {
    // val tag = UInt(9.W) old version
    val tag = UInt(12.W)
    val target = UInt(VAddrBits.W)
    val _type = UInt(2.W)
    // val crosslineJump = Bool()
    val valid = Bool()
  }

  ///////////////modifuy!!///////////////////////
  val icacheLine = 4//x2 byte
  val btbSizePerBank = NRbtb/icacheLine
  val btbAddrWidth = log2Ceil(btbSizePerBank)

  val bhtSizePerBank = NRbht/icacheLine
  val bhtAddrWidth = log2Ceil(bhtSizePerBank)
  val mergedGhr = Wire(UInt(5.W))
  val fghr = RegInit(0.U(5.W))
  val fghrNextState = WireInit(0.U(5.W))
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)

  /******************************BTB region******************************/
  val btbRdAddr = btbAddr.hashBTBAddr(io.in.pc.bits)
  // val bhtRdAddr = io.in.pc.bits(7,4)
  val bhtRdAddr = hashBhtAddr(io.in.pc.bits,btbRdAddr,fghrNextState)  //fix bug

  val btbWrAddr = WireInit(0.U(btbAddrWidth.W))
  val btbWrData = WireInit(0.U.asTypeOf(btbEntry()))
  val btbWrEWay0 = Wire(UInt(icacheLine.W))
  val btbWrEWay1 = Wire(UInt(icacheLine.W))


  val btbList = VecInit(Seq.tabulate(btbSizePerBank)(i => (
    VecInit(Seq.tabulate(icacheLine)( j => (
      RegEnable(btbWrData, 0.U.asTypeOf(btbEntry()), ( btbWrAddr === i.U )&& btbWrEWay0(j))
    )))
  )))

  dontTouch(btbList)


  val btbTwoWaysRegOut = List.tabulate(icacheLine)(j => (
      RegEnable(btbList(btbRdAddr)(j),true.B)
    ))

  val tagMatchWay0 = Wire(Vec(icacheLine,Bool()))
  
  (0 to icacheLine-1).map(i => (
    tagMatchWay0(i) := btbTwoWaysRegOut(i).valid && !flush && (btbTwoWaysRegOut(i).tag === (pcLatch(15,4)))
  ))

  //both way could hit
  val finalBtbRes = List.fill(icacheLine)(Wire(UInt()))
  (0 to icacheLine-1).map(i => (
    finalBtbRes(i) := Fill(58,tagMatchWay0(i)) & btbTwoWaysRegOut(i).asUInt 
  ))
  val wayHit = Wire(Vec(icacheLine,Bool()))
  (0 to icacheLine-1).map(i => (
    wayHit(i) := tagMatchWay0(i) 
  ))
  
  /**********************BTB region end********************************8***/

/**************************update region****************************/
  val req  = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  val i0wb = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  val i1wb = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  BoringUtils.addSink(req, "mpbpuUpdateReq")
  BoringUtils.addSink(i0wb, "i0WbBpuUpdateReq")
  BoringUtils.addSink(i1wb, "i1WbBpuUpdateReq")

  val mpBank = req.pc(2,1)
  val mpBtbIndex = btbAddr.hashBTBAddr(req.pc)


  val mpBtbWay = 0.U      /////default !!!
  val mpValid = req.valid & req.isMissPredict
  val mpType = req.btbType
  val mpActualTaken = req.actualTaken
  val mpActualTarget = req.actualTarget

  val mpWriteValid = mpActualTaken & mpValid //if mispredict occurs and not taken, do not update BTB
  // val mpWriteValid = mpValid

  btbWrAddr := mpBtbIndex
  btbWrEWay0 := Fill(icacheLine,~mpBtbWay & mpWriteValid) & decode24(mpBank)
  btbWrEWay1 := Fill(icacheLine, mpBtbWay & mpWriteValid) & decode24(mpBank)

  
  
  ////bht update
  //mpbank is on upward
  val i0Bank = i0wb.pc(2,1)
  val i1Bank = i1wb.pc(2,1)

  val bhtWrEMp = Wire( UInt(icacheLine.W) ) 
  val bhtWrE1 = Wire( UInt(icacheLine.W) )
  val bhtWrE2 = Wire( UInt(icacheLine.W) )

  // Experiments show this is the best priority scheme for same bank/index writes at the same time.
  
  
  
  /***********************update region end**************************/
  
  val eghr = req.ghrFetch
  val bhtWrAddr0 = hashBhtAddr(req.pc,btbWrAddr,eghr)
  val bhtWrAddr1 = hashBhtAddr(i0wb.pc,btbAddr.hashBTBAddr(i0wb.pc),i0wb.ghrFetch)
  val bhtWrAddr2 = hashBhtAddr(i1wb.pc,btbAddr.hashBTBAddr(i1wb.pc),i1wb.ghrFetch)
  
  val bhtValid = wayHit

  val bhtBankSel = List.tabulate(bhtSizePerBank)(i => (
    List.tabulate(icacheLine)( j => (
      Wire(Bool())
    ))
  ))
  
  val bhtMpNewCnt = Wire(UInt(2.W))
  val bhtD1NewCnt = Wire(UInt(2.W))
  val bhtD2NewCnt = Wire(UInt(2.W))



  val bhtList = VecInit(Seq.tabulate(bhtSizePerBank)(i => (
    VecInit(Seq.tabulate(icacheLine)(j => (
      RegEnable(Mux((bhtWrAddr1 === i.U) && bhtWrE1(j) ,bhtD1NewCnt,
        Mux((bhtWrAddr2 === i.U) &&bhtWrE2(j) ,bhtD2NewCnt,
        bhtMpNewCnt)
      ), 0.U(2.W),bhtBankSel(i)(j))
    )))
  )))
  dontTouch(bhtList)

  val bhtMpOricnt = bhtList(bhtWrAddr0)(mpBank)
  val bhtD1OriCnt = bhtList(bhtWrAddr1)(i0Bank)
  val bhtD2OriCnt = bhtList(bhtWrAddr2)(i1Bank)

  bhtMpNewCnt := Mux(mpActualTaken,    Mux(bhtMpOricnt === "b11".U,bhtMpOricnt,bhtMpOricnt+1.U),Mux(bhtMpOricnt === "b00".U,bhtMpOricnt,bhtMpOricnt-1.U) )
  bhtD1NewCnt := Mux(i0wb.actualTaken, Mux(bhtD1OriCnt === "b11".U,bhtD1OriCnt,bhtD1OriCnt+1.U),Mux(bhtD1OriCnt === "b00".U,bhtD1OriCnt,bhtD1OriCnt-1.U) )
  bhtD2NewCnt := Mux(i1wb.actualTaken, Mux(bhtD2OriCnt === "b11".U,bhtD2OriCnt,bhtD2OriCnt+1.U),Mux(bhtD2OriCnt === "b00".U,bhtD2OriCnt,bhtD2OriCnt-1.U) )

  
  (0 to bhtSizePerBank-1).map(i => (
    (0 to icacheLine-1).map( j=> (
      bhtBankSel(i)(j) := (bhtWrAddr0 === i.U) && bhtWrEMp(j) || (bhtWrAddr1 === i.U) && bhtWrE1(j) || (bhtWrAddr2 === i.U) && bhtWrE2(j)
    ))
  ))

  val bhtRegOut = List.fill(icacheLine)(
    Wire(UInt(2.W))
  )

  /////////////////////valid be setted to true.B
  (0 to icacheLine-1).map(j => (
    bhtRegOut(j) := RegEnable(bhtList(bhtRdAddr)(j), true.B)
  ))

  val bhtTaken = Wire(Vec(icacheLine,Bool()))
  (0 to icacheLine-1).map(i => (
    bhtTaken(i) := bhtRegOut(i)(1)
  ))

  /////what the mean of these code?
  val bhtDir = Wire(Vec(icacheLine,Bool()))
  val bhtForceTaken = Wire(Vec(icacheLine, Bool()))
  (0 to icacheLine-1).map(i => (
    bhtForceTaken(i) := ((finalBtbRes(i).asTypeOf(btbEntry())).valid) && (((finalBtbRes(i).asTypeOf(btbEntry()))._type === BTBtype.C) || ((finalBtbRes(i).asTypeOf(btbEntry()))._type === BTBtype.J) || ((finalBtbRes(i).asTypeOf(btbEntry()))._type === BTBtype.R) )
  ))

  // bhtDir :=(forceTaken ^ bhtTaken) & wayHit
  (0 to icacheLine-1).map(i => ( 
    bhtDir(i) := (bhtTaken(i) | bhtForceTaken(i)) & wayHit(i)
  ))
   //ras
  val NRras = 16
  val ras = Mem(NRras, UInt(VAddrBits.W))
  val sp = Counter(NRras)
  val rasTarget = RegEnable(ras.read(sp.value), io.in.pc.valid)

  val target = Wire(Vec(icacheLine, UInt(VAddrBits.W)))
  val brIdx = Wire(Vec(icacheLine,Bool()))


  val pcLatchValid = genInstValid(pcLatch)
 
  //RAS speculative update
  val brIdxOneHot = UIntToOH(brIdx.asUInt())
  val retIdx = VecInit(Seq.fill(icacheLine)(false.B))
  val retPC = Mux1H(brIdxOneHot,Seq(Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+4.U,Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+6.U,Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+8.U,Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+10.U,Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+12.U,Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+14.U,Cat(pcLatch(VAddrBits-1,4),0.U(4.W))+16.U,Cat(pcLatch(VAddrBits-1,4),0.U(3.W))+18.U))
  (0 to icacheLine-1).map(i => retIdx(i) := ((finalBtbRes(i).asTypeOf(btbEntry()))._type === BTBtype.C) && (brIdxOneHot(i)))
  val rasWen = retIdx.asUInt.orR()
  val rasEmpty = sp.value === 0.U

  val retRetire = WireInit(false.B)
  BoringUtils.addSink(retRetire,"backendRetRetire")
  when (rasWen)  {
    ras.write(sp.value + 1.U, retPC)  //TODO: modify for RVC
    sp.value := sp.value + 1.U
  }.elsewhen(retRetire){
    when(sp.value === 0.U) {
        // RAS empty, do nothing
      }
    sp.value := Mux(sp.value===0.U, 0.U, sp.value - 1.U)
  }.elsewhen (req.valid && req.fuOpType === ALUOpType.ret) {
      when(sp.value === 0.U) {
        // RAS empty, do nothing
      }
      sp.value := Mux(sp.value===0.U, 0.U, sp.value - 1.U)
  }


  // (0 to icacheLine-1).map(i => brIdx(i) := pcLatchValid(i).asBool && Mux(finalBtbRes(i).asTypeOf(btbEntry())._type === BTBtype.R, !rasEmpty, bhtDir(i) ) )
  (0 to icacheLine-1).map(i => brIdx(i) := pcLatchValid(i).asBool && bhtDir(i) ) 

  io.brIdx := outputHold(brIdx,validLatch) 

  io.crosslineJump := false.B

  /*
  if the fetch mask & instvalid === "b0000".U
  
  */

  (0 to icacheLine-1 ).map(i => target(i) := Mux((finalBtbRes(i).asTypeOf(btbEntry()))._type === BTBtype.R && !rasEmpty, rasTarget, (finalBtbRes(i).asTypeOf(btbEntry())).target))

  io.saveTheFetch := 0.U
  io.saveAddr := pcLatch + 8.U
  io.out.target := outputHold(PriorityMux(brIdx,target), validLatch)
  io.out.valid :=outputHold( brIdx.asUInt.orR, validLatch)

  io.out.rtype := 0.U
  io.out.pc:=io.in.pc.bits //not used

  io.out.ghrUpdate := 0.U
  io.out.ghrUpdateValid :=0.U
  val btbIsBranch = Wire(Vec(icacheLine,Bool()))
  (0 to icacheLine-1).map(i => (
    // btbIsBranch(i) := finalBtbRes(i).asTypeOf(btbEntry()).valid && (finalBtbRes(i).asTypeOf(btbEntry())._type === BTBtype.B)
    btbIsBranch(i) := wayHit(i)
  ))
  io.out.btbIsBranch := outputHold(btbIsBranch.asUInt(),validLatch)
  ////////////////////////////////
  

  // btbWrData.tag :=  req.pc(23,15) ^ req.pc(14,6)
  btbWrData.tag :=  req.pc(15,4)

  btbWrData.target := mpActualTarget
  btbWrData._type := mpType
  // btbWrData.crosslineJump := 0.U
  btbWrData.valid := true.B

  bhtWrEMp := Fill(icacheLine , req.valid & ~(req.fuOpType === BTBtype.R) & ~(req.fuOpType === BTBtype.J)& ~(req.fuOpType === BTBtype.C)) & decode24(mpBank)
  bhtWrE1  := Fill(icacheLine , i0wb.valid && ~(i0wb.fuOpType === BTBtype.R) & ~(i0wb.fuOpType === BTBtype.J)& ~(i0wb.fuOpType === BTBtype.C)) & decode24(i0Bank)
  bhtWrE2  := Fill(icacheLine , i1wb.valid && ~(i1wb.fuOpType === BTBtype.R) & ~(i1wb.fuOpType === BTBtype.J)& ~(i1wb.fuOpType === BTBtype.C)) & decode24(i1Bank)


  ////addation
  val count=0;
  if(SSDCoreConfig().EnablePerfCnt){

    

  } 






  // ghr
  val ghrUpdate = WireInit(0.U(GhrLength.W))
  BoringUtils.addSink(ghrUpdate,"ghrUpdate")
  fghrNextState := Mux(mpValid,ghrUpdate,Mux(validLatch,mergedGhr,fghr))
  when(true.B){
    fghr := fghrNextState
  }
  io.fghr := fghr
  val brIdxPri = Mux(brIdx(0),"b0001".U,
  Mux(brIdx(1),"b0011".U,
  Mux(brIdx(2),"b0111".U,"b1111".U)))

  val brIdxMask = Wire(UInt(icacheLine.W))
  brIdxMask := pcLatchValid & brIdxPri

  val tmp = (bhtValid.asUInt & brIdxMask.asUInt)

  val numValids = count(bhtValid.asUInt & brIdxMask.asUInt)
  // val ghrNs = Mux(exuF,2,3,)
  mergedGhr := Mux(numValids >= "d4".U, Cat(fghr(4), 0.U(3.W), tmp.orR),
  Mux(numValids === "d3".U, Cat(fghr(4,3), 0.U(2.W), tmp.orR),
  Mux(numValids === "d2".U, Cat(fghr(2,0), 0.U(1.W), tmp.orR),
  Mux(numValids === "d1".U, Cat(fghr(3,0), tmp.orR),fghr
  ))))




  dontTouch(mergedGhr)

  //bht access 
  val SSDcoretrap = WireInit(false.B)
  val space = bhtSizePerBank
  BoringUtils.addSink(SSDcoretrap,"SSDcoretrap")
  val bhtCnts = List.fill(space)(RegInit(0.U(64.W)))
  val total = RegInit(0.U(64.W))
  if(SSDCoreConfig().EnableBPUCnt){
    // for(i <- 0 to bhtSizePerBank-1){

    //   when((bhtRdAddr === i.U)  && io.in.pc.valid){
    //     bhtCnts(i) := bhtCnts(i) + 1.U
    //     total := total+1.U
    //   }
    // }
    // when(RegNext(SSDcoretrap)) {
    //   (0 to bhtSizePerBank-1).map { i => {printf( " %d access ->  %d\n",i.U, bhtCnts(i))}
    //   printf("total access -> %d\n ",total)
    //   }
    // }
  }
}

