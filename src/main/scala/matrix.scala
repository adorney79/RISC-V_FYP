package risc_v_fyp
import freechips._
import Chisel._
import chisel3.util.{HasBlackBoxResource}
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}
import chisel3.experimental.Param
import Chisel.ImplicitConversions._
import freechips.rocketchip.rocket._
import chisel3.VecInit
import chisel3.chiselTypeOf

class MatrixRoCC(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new MatrixModule(this)
}
class MatrixModule(outer: MatrixRoCC)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
  //val regfile = Mem(4, UInt(xLen.W))
  // val busy = RegInit(VecInit(Seq.fill(4){false.B}))
  val busy=RegInit(false.B)
  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct

  val len=cmd.bits.rs2
  val doWrite = funct === 0.U
  val doRead = funct === 1.U
  val doLoad = funct === 2.U
  val doAccum = funct === 3.U
  val address=cmd.bits.rs1
  val result=RegInit(0.U(64.W))
  val stallLoad = doLoad && !io.mem.req.ready
  val done=RegInit(false.B)
  val doResp = cmd.bits.inst.xd
  val stallResp = doResp && !io.resp.ready
  val sending :: reading  ::ready::idle::receiving::Nil = Enum(5)
  val current = RegInit(0.U(64.W)) // value index
  val state = RegInit(sending)
  val stop=RegInit(0.U(64.W))
  // io.mem.req.valid := cmd.valid && doLoad && !busy && !stallResp
  when(doLoad && !busy){
      current:=address
      stop:=address+(len*8)
    }
  when (io.mem.req.fire()) {
    busy := true.B
  }
  io.mem.req.valid:=  (!done) && current=/=0.U && stop=/=0.U
  // io.mem.req.bits.addr := address
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U // we're not performing any stores...
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
  //cmd.ready := !busy 
  cmd.ready := !busy && !stallLoad && !stallResp
  io.resp.valid := cmd.valid && doResp && !busy && done
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := result

  io.busy := busy
  io.interrupt := false.B
  when(io.mem.req.valid){
    switch(state){
      is(reading){
          result:=result+io.mem.resp.bits.data        
          state:=sending
        
      }
      is(sending){
        when(current>stop){
          state:=idle
        }.otherwise{
            io.mem.req.bits.addr := current
            state:=receiving
          }
       }
  
    is(receiving){
          when(io.mem.resp.valid){
            current:=current+8.U
            state:=reading
          }
    }
    is(idle){
      busy:=false.B
      done:=true.B
      }
}
}

}




class WithMatrixRoCC extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new MatrixRoCC(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})