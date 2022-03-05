package risc_v_fyp

import freechips._
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
import chisel3.util._
import chisel3._

class RegexRoCC(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new RegexModule(this)
}

class WithRegexRoCC extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new RegexRoCC(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})
class RegexModule(outer: RegexRoCC)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
  val cmd = Queue(io.cmd)

  //val matcher=Module(new CharMatch(97.U))
  

  val current=RegInit(0.U(64.W))
  val funct = cmd.bits.inst.funct
  val address=RegInit(0.U(64.W))
  val letsgo = funct === 1.U
  val doResp=funct===2.U
  val stallResp = doResp && !io.resp.ready

  
  val idle::read_char::receive_char::matching::ready::Nil=Enum(5)
  val state=RegInit(idle)
  val busy=RegInit(false.B)

  when (io.mem.req.fire) {
    busy := true.B
  }
 // io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height
   io.mem.req.valid:=  state===read_char
  // io.mem.req.bits.addr := address
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U // we're not performing any stores...
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
  //cmd.ready := !busy  
  cmd.ready := !busy && !stallResp
  io.resp.valid := cmd.valid && doResp && !busy
  io.resp.bits.rd := cmd.bits.inst.rd
  //io.resp.bits.data := current
  io.resp.bits.data := 0.U

  io.busy := busy
  io.interrupt := false.B

  //matcher.io.req.valid:=(state==matching)
  //matcher.io.resp.ready:=(state==matching)
  //matcher.io.req.bits.value:=current

  when(letsgo && !busy){
    address:=cmd.bits.rs1
    //state:=read_char
  }
  // when (io.mem.req.fire) {
  //   busy := true.B
  // }

  // when(io.resp.fire){
  //   state:=idle
  // }
  switch(state){
    is(idle){
      when(address =/= 0.U){
        busy := true.B
        state:=read_char
      }
    }
      is(read_char){
        when(io.mem.req.valid && io.mem.req.ready){
          state:=receive_char
        }
      }
      is(receive_char){
         when(io.mem.resp.valid){
          current:=io.mem.resp.bits.data
          state:=matching
        }
      }
      is(matching){
        state:=ready
        busy:=false.B
        // when(matcher.io.resp.ready && matcher.io.resp.valid){
        //   io.resp.bits.data:=matcher.io.resp.bits.matched
        //   state:=ready
        //   busy:=false.B
        // }
      }
      is(ready){
          when(io.resp.fire){
            state:=idle
          }
      }
  }

}

class CharMatch(input:UInt) extends Module{
  val io = IO(new Bundle {
    val req=Flipped(Decoupled(new Bundle{
      val value=Input(UInt(8.W))
      //value:=0.U
    }))
    val resp=Decoupled(new Bundle{
      val matched=Output(Bool())
    })
  })
  val dummy1 =Wire(Bool())
  dummy1:=false.B
  io.resp.bits.matched:=dummy1
  val idle::matching::matched::Nil=Enum(3)
  val state=RegInit(idle)
  io.req.ready := (state == idle)
  io.resp.valid:=(state==matched)
  when(io.req.fire){
    dummy1:=true.B
    state:=matching
  }
 
  switch(state){
    is(idle){
        
    }
    is(matching){
      when(io.req.bits.value==input){
        io.resp.bits.matched:=true.B
        state:=idle
      }
    }

  }

}