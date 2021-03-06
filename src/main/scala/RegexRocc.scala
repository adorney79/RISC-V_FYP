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
  val mem=Mem(3000,UInt(8.W))
  val current=RegInit(0.U(64.W))
  val funct = cmd.bits.inst.funct
  val start=RegInit(0.U(64.W))
  val stop=RegInit(0.U(64.W))
  val pointer=RegInit(0.U(64.W))
  val holder=RegInit(0.U(8.W))
  val backtrack=RegInit(0.U(64.W))
  val backtrack2=RegInit(0.U(64.W))
  val backtrack3=RegInit(0.U(64.W))
  val success=RegInit(false.B)
  val done=RegInit(false.B)
  val dont_care=RegInit(false.B)
  val counter=RegInit(0.U(32.W))
  val overflow=RegInit(0.U(8.W))


  val resp_tag=io.mem.resp.bits.tag
  val set_addr = funct === 1.U
  val doResp=(funct===2.U) 
  val doLoad = funct === 4.U

  val stallLoad = doLoad && !io.mem.req.ready
  val stallResp = doResp && !io.resp.ready
  //a vector for holding all of our match symbols
  val lst=VecInit('S'.toInt.U,'&'.toInt.U,'a'.toInt.U,'m'.toInt.U,'p'.toInt.U,';'.toInt.U,'P'.toInt.U,
                  ' '.toInt.U,'5'.toInt.U,'0'.toInt.U,'0'.toInt.U,'"'.toInt.U,'r'.toInt.U,'e'.toInt.U,
                  'g'.toInt.U,'u'.toInt.U,'l'.toInt.U,'a'.toInt.U,'r'.toInt.U,'M'.toInt.U,'a'.toInt.U,
                  'r'.toInt.U,'k'.toInt.U,'e'.toInt.U,'t'.toInt.U,'P'.toInt.U,'r'.toInt.U,'i'.toInt.U,
                  'c'.toInt.U,'e'.toInt.U,'v'.toInt.U,'a'.toInt.U,'l'.toInt.U,'u'.toInt.U,'e'.toInt.U,'='.toInt.U,
                  '"'.toInt.U)

  val lst_idx=RegInit(0.U(32.W))
  val lst_len=37.U
  val value=mem(pointer)
  val idle::read_char::receive_char::m1::m2::match_any1::match_any2::potential1::potential2::ready::Nil=Enum(10)
  val state=RegInit(idle)
  val busy=RegInit(false.B)
  when (io.mem.req.fire) {
    busy := true.B
  }
  
  io.mem.req.valid:=  (state===read_char) && (io.mem.req.bits.addr <= stop) && !done && counter=/=63.U
  io.mem.req.bits.addr:=start+counter+(overflow*63.U)
  io.mem.req.bits.tag := counter
  io.mem.req.bits.cmd := M_XRD //LOAD
  io.mem.req.bits.size := 0.U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U 
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
  cmd.ready := !busy && !stallLoad && !stallResp && start=/=0.U
  io.resp.valid := cmd.valid && doResp && !busy
  io.resp.bits.rd := cmd.bits.inst.rd
  
  io.resp.bits.data:=success

  io.busy := busy
  io.interrupt := false.B

 //"S&amp;P 500".*?regularMarketPrice.*?value=" 

 //used to begin operation and store start and end of string
  when(set_addr && !busy && !done){
    start:=cmd.bits.rs1
    stop:=cmd.bits.rs2
    busy:=true.B
  }

  switch(state){
    is(idle){
      when(start =/= 0.U){
        busy := true.B
        state:=read_char
      }
    }
      is(read_char){
        //read in values from memory
        when(io.mem.req.valid && io.mem.req.ready){
          counter:=counter+1.U

        }
        //used to prevent tag from overflowing 
        when(counter===63.U){
                when(resp_tag===62.U){
                  overflow:=overflow+1.U
                  counter:=0.U
                }
              }
        //store values in internal memory
        mem(resp_tag+(overflow*63.U)):=io.mem.resp.bits.data
        when(!io.mem.req.valid && !io.mem.resp.valid && counter=/=63.U){
            counter:=0.U
            state:=m1
            holder:=mem(pointer)
            
          }
      }
      //3
      //continues searching till EOF or first value matched
      is(m1){
        holder:=mem(pointer)
        when(holder=== lst(0.U)){
          backtrack:=pointer
          pointer:=pointer +1.U
          lst_idx:=lst_idx+1.U
          state:=m2

        }.elsewhen(pointer>(stop-start)){
            busy:=false.B
            done:=true.B
            state:=ready
        }.otherwise{
        pointer:=pointer+1.U
        }

        
        
      
      }
      //4
      //matches rest of word or backtracks
      is(m2){
        holder:=mem(pointer)
        //if matched go to first ".*?" stage
        when(lst_idx===12.U){
          state:=match_any1
          pointer:=pointer-1.U
        }
        .elsewhen(holder===lst(lst_idx)){
          pointer:=pointer+1.U
          lst_idx:=lst_idx+1.U
        }.otherwise{
          pointer:=backtrack 
          lst_idx:=0.U
          state:=m1
        }
      }
    //5
    //matches any character till EOF or potential match
     is(match_any1){
        holder:=mem(pointer)
        when(pointer>(stop-start)){
          pointer:=backtrack 
          lst_idx:=0.U
          state:=m1
        }
        .elsewhen(holder===lst(12.U)){
          state:=potential1
          pointer:=pointer+1.U
          lst_idx:=lst_idx+1.U
          backtrack2:=pointer
        }.otherwise{
        pointer:=pointer+1.U
        }

     }
     //7
     //checks potential match and either backtracks or
     // proceeds to second ".*?" stage
     is(potential1){
       holder:=mem(pointer)
        when(lst_idx===30.U){
          state:=match_any2
          pointer:=pointer-1.U
        }
        .elsewhen(holder===lst(lst_idx)){
          pointer:=pointer+1.U
          lst_idx:=lst_idx+1.U
        }.otherwise{
          pointer:=backtrack2 
          lst_idx:=12.U
          state:=match_any1
        }
     }
     //6
     //matches any character till EOF or potential match
     is(match_any2){
        holder:=mem(pointer)
        when(pointer>(stop-start)){
          pointer:=backtrack2 
          lst_idx:=12.U
          state:=match_any1
        }
        .elsewhen(holder===lst(30.U)){
          state:=potential2
          pointer:=pointer+1.U
          lst_idx:=lst_idx+1.U
          backtrack3:=pointer
        }.otherwise{
        pointer:=pointer+1.U
        }

     }
     //8
     //if a match is found then stop parsing and proceed to ready state.
     // otherwise backtrack
      is(potential2){
       holder:=mem(pointer)
        when(lst_idx===lst_len){
          success:=true.B
          done:=true.B
          busy:=false.B
          state:=ready
        }
        .elsewhen(holder===lst(lst_idx)){
          pointer:=pointer+1.U
          lst_idx:=lst_idx+1.U
        }.otherwise{
          pointer:=backtrack3 
          lst_idx:=30.U
          state:=match_any2
        }
     }
      is(ready){
          when(io.resp.fire){
            start:=0.U
            state:=idle
          }
      }
  }

}
//unused class for abstacting the logic of matching a character
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




