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
class MatrixRoCC2(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new MatrixModule2(this)
}
class MatrixModule(outer: MatrixRoCC)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
  //val regfile = Mem(4, UInt(xLen.W))
  // val busy = RegInit(VecInit(Seq.fill(4){false.B}))
  val busy=RegInit(false.B)
  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct

  val len=cmd.bits.rs2
  val doXdim = funct === 0.U
  val doYdim = funct === 1.U
  val doAddress = funct === 2.U
  val doOut = funct === 3.U
  val doLoad = funct === 4.U
  val doRead = funct === 5.U
  val address=cmd.bits.rs1
  val result=RegInit(0.U(64.W))
  val stallLoad = doLoad && !io.mem.req.ready
  val done=RegInit(false.B)
  val doResp = cmd.bits.inst.xd
  val stallResp = doResp && !io.resp.ready
  //val sending :: reading  ::ready::idle::receiving::Nil = Enum(5)
  val offset=8.U
  val idle::read_row::recv_row::read_col::recv_col::calculate::write::inter::Nil=Enum(8)
  val state = RegInit(idle)
  val current = RegInit(0.U(64.W)) 
  val stop=RegInit(0.U(64.W))
  val x_len=RegInit(0.U(64.W))
  val y_len=RegInit(0.U(64.W))
  val x_height=RegInit(0.U(64.W))
  val y_height=RegInit(0.U(64.W))
  val x_addr=RegInit(0.U(64.W))
  val y_addr=RegInit(0.U(64.W))
  val z_addr=RegInit(0.U(64.W))
  val write_val=RegInit(0.U(64.W))
  val current_row=RegInit(0.U(64.W))
  val current_col=RegInit(0.U(64.W))
  val row_idx=RegInit(0.U(64.W))
  val row_val=RegInit(0.U(64.W))
  // io.mem.req.valid := cmd.valid && doLoad && !busy && !stallResp
  when(doXdim && !busy){
    x_len:=cmd.bits.rs1
    x_height:=cmd.bits.rs2
  }
  when(doYdim && !busy){
    y_len:=cmd.bits.rs1
    y_height:=cmd.bits.rs2
  }
  when(doAddress && !busy){
    x_addr:=cmd.bits.rs1
    y_addr:=cmd.bits.rs2
  }
  when(doOut && !busy){
    z_addr:=cmd.bits.rs1
  }

  when (io.mem.req.fire()) {
    busy := true.B
  }
  io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height
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
  io.resp.valid := cmd.valid && doResp && !busy && done && io.mem.resp.bits.data===0.U
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := current

  io.busy := busy
  io.interrupt := false.B
  switch(state){
      is(idle){
        when(z_addr=/=0.U){
          state:=read_row
          busy:=true.B
        }
      }
      is(read_row){
        when(current_row===x_height){
          z_addr:=0.U
          done:=true.B
          busy:=false.B
          state:=idle
        }.otherwise{
            io.mem.req.bits.cmd:=M_XRD
            io.mem.req.bits.addr:=(x_len*offset*current_row)+row_idx*offset+x_addr
            when(io.mem.req.valid && io.mem.req.ready){
                state:=recv_row
            }
        }
      }
      is(recv_row){
        when(io.mem.resp.valid){
            row_val:=io.mem.resp.bits.data
            state:=read_col
        }
      }
        is(read_col){
            io.mem.req.bits.cmd:=M_XRD
            io.mem.req.bits.addr:=(y_len*offset)*row_idx+current_col*offset+y_addr
            when(io.mem.req.valid && io.mem.req.ready){
                state:=recv_col
            }
        }
        is(recv_col){
          when(io.mem.resp.valid){
            row_val:=row_val*io.mem.resp.bits.data
            row_idx:=row_idx+1.U
            state:=calculate
        }
        }
        is(calculate){
          current:=current+row_val
            when(row_idx===x_len){
              state:=write
              row_idx:=0.U
              // current_col:=current_col+1.U
            }.otherwise{
              state:=read_row
            }
        }
        is(write){
          io.mem.req.bits.cmd:=M_XWR
          io.mem.req.bits.data:=current
          io.mem.req.bits.addr:=z_addr+x_len*offset*current_row+current_col*offset
          when(io.mem.resp.valid){
            state:=inter
          }
        }
        is(inter){
          current:=0.U
            when(current_col<(y_len-1.U)){
              current_col:=current_col+1.U
            }.otherwise{
                current_row:=current_row+1.U
                current_col:=0.U
            }
          state:=read_row
        }
      }
    }
class MatrixModule2(outer: MatrixRoCC2)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
  //val regfile = Mem(4, UInt(xLen.W))
  val rows = Mem(64,UInt(64.W))
  val cols = Mem(4096,UInt(64.W))
  val stored=RegInit(false.B)
  val counter=RegInit(0.U(log2Ceil(64).W))
  val busy=RegInit(false.B)
  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct

  val len=cmd.bits.rs2
  val doXdim = funct === 0.U
  val doYdim = funct === 1.U
  val doAddress = funct === 2.U
  val doOut = funct === 3.U
  val doLoad = funct === 4.U
  val doRead = funct === 5.U
  val address=cmd.bits.rs1
  val result=RegInit(0.U(64.W))
  val stallLoad = doLoad && !io.mem.req.ready
  val done=RegInit(false.B)
  val doResp = cmd.bits.inst.xd
  val stallResp = doResp && !io.resp.ready
  //val sending :: reading  ::ready::idle::receiving::Nil = Enum(5)
  val offset=8.U
  val idle::read_row::recv_row::read_col::recv_col::calculate::write::inter::Nil=Enum(8)
  val state = RegInit(idle)
  val current = RegInit(0.U(64.W)) 
  val stop=RegInit(0.U(64.W))
  val x_len=RegInit(0.U(64.W))
  val y_len=RegInit(0.U(64.W))
  val x_height=RegInit(0.U(64.W))
  val y_height=RegInit(0.U(64.W))
  val x_addr=RegInit(0.U(64.W))
  val y_addr=RegInit(0.U(64.W))
  val z_addr=RegInit(0.U(64.W))
  val write_val=RegInit(0.U(64.W))
  val current_row=RegInit(0.U(64.W))
  val current_col=RegInit(0.U(64.W))
  val row_idx=RegInit(0.U(64.W))
  val row_val=RegInit(0.U(64.W))
  val resp_tag=io.mem.resp.bits.tag

  // io.mem.req.valid := cmd.valid && doLoad && !busy && !stallResp
  when(doXdim && !busy){
    x_len:=cmd.bits.rs1
    x_height:=cmd.bits.rs2
  }
  when(doYdim && !busy){
    y_len:=cmd.bits.rs1
    y_height:=cmd.bits.rs2
  }
  when(doAddress && !busy){
    x_addr:=cmd.bits.rs1
    y_addr:=cmd.bits.rs2
  }
  when(doOut && !busy){
    z_addr:=cmd.bits.rs1
  }

  when (io.mem.req.fire()) {
    busy := true.B
  }
  io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height
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
  io.resp.valid := cmd.valid && doResp && !busy && done && io.mem.resp.bits.data===0.U
  io.resp.bits.rd := cmd.bits.inst.rd
  //io.resp.bits.data := current
    io.resp.bits.data := rows(1.U)

  io.busy := busy
  io.interrupt := false.B
  switch(state){
      is(idle){
        when(z_addr=/=0.U){
          state:=read_row
          busy:=true.B
          io.mem.req.bits.cmd:=M_XRD

        }
      }
      is(read_row){
        io.mem.req.bits.addr:=(x_len*offset*current_row)+row_idx*offset+x_addr
        io.mem.req.bits.tag:=row_idx
        when(current_row===x_height){
          z_addr:=0.U
          done:=true.B
          busy:=false.B
          state:=idle
        }.otherwise{
            when(row_idx===x_len){
              io.mem.req.valid:=false.B
            }.elsewhen(io.mem.req.valid && io.mem.req.ready){
              row_idx:=row_idx+1.U
            }
            // }.elsewhen(io.mem.req.valid && io.mem.req.ready){
            //     io.mem.req.bits.cmd:=M_XRD
            //     io.mem.req.bits.addr:=(x_len*offset*current_row)+row_idx*offset+x_addr
            //     io.mem.req.bits.tag:=row_idx
            //     row_idx:=row_idx+1.U
            // }
      
          rows(resp_tag):=io.mem.resp.bits.data
          // counter:=counter+1.U
      
          when(!io.mem.req.valid && !io.mem.resp.valid){
            //io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height
            state:=Mux(stored,calculate,read_col)
            counter:=0.U
            row_idx:=0.U
            
          }
        }

      }

        is(read_col){
            io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height
            io.mem.req.bits.addr:=(y_len*offset)*row_idx+current_col*offset+y_addr
            io.mem.req.bits.tag:=counter
            when(io.mem.req.valid && io.mem.req.ready){
              counter:=counter+1.U
                when(row_idx===y_height-1.U && current_col===y_len-1.U ){
                   state:=recv_col
                }.elsewhen(row_idx===y_height-1.U){
                  current_col:=current_col+1.U
                  row_idx:=0.U
                }.otherwise{
                  row_idx:=row_idx+1.U
                }

            }
          cols(resp_tag):=io.mem.resp.bits.data
      

        }
        is(recv_col){
            cols(resp_tag):=io.mem.resp.bits.data
        
        when(!io.mem.resp.valid){
          io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height
          stored:=true.B
          state:=calculate
          current_col:=0.U
          row_idx:=0.U
          counter:=0.U
        }
        }
        is(calculate){
          when(row_idx===x_len){
            state:=write
            row_idx:=0.U
          }.elsewhen(current_col===y_len){
            io.mem.req.bits.cmd:=M_XRD
            current_col:=0.U
            current_row:=current_row+1.U
            state:=read_row
          }
          .otherwise{
            current:=current+(rows(row_idx)*cols(row_idx+(current_col*y_len)))
            row_idx:=row_idx+1.U
          }
        }
        is(write){
          io.mem.req.bits.cmd:=M_XWR
          io.mem.req.bits.data:=current
          io.mem.req.bits.addr:=z_addr+x_len*offset*current_row+current_col*offset
          when(io.mem.resp.valid){
            current_col:=current_col+1.U
            current:=0.U
            state:=calculate
          }
        }

      }
    }
//   when(io.mem.req.valid){
//     switch(state){
//       is(reading){
//           result:=result+io.mem.resp.bits.data        
//           state:=sending
        
//       }
//       is(sending){
//         when(current>stop){
//           state:=idle
//         }.otherwise{
//             io.mem.req.bits.addr := current
//             state:=receiving
//           }
//        }
  
//     is(receiving){
//           when(io.mem.resp.valid){
//             current:=current+8.U
//             state:=reading
//           }
//     }
//     is(idle){
//       busy:=false.B
//       done:=true.B
//       }
// }
// }

//}
// class SumReduceModule(outer: MatrixRoCC)(implicit p: Parameters)
// extends LazyRoCCModuleImp(outer) with HasCoreParameters{
//   //val regfile = Mem(4, UInt(xLen.W))
//   // val busy = RegInit(VecInit(Seq.fill(4){false.B}))
//   val busy=RegInit(false.B)
//   val cmd = Queue(io.cmd)
//   val funct = cmd.bits.inst.funct

//   val len=cmd.bits.rs2
//   val doWrite = funct === 0.U
//   val doRead = funct === 1.U
//   val doLoad = funct === 2.U
//   val doAccum = funct === 3.U
//   val address=cmd.bits.rs1
//   val result=RegInit(0.U(64.W))
//   val stallLoad = doLoad && !io.mem.req.ready
//   val done=RegInit(false.B)
//   val doResp = cmd.bits.inst.xd
//   val stallResp = doResp && !io.resp.ready
//   val sending :: reading  ::ready::idle::receiving::Nil = Enum(5)
//   val current = RegInit(0.U(64.W)) // value index
//   val state = RegInit(sending)
//   val stop=RegInit(0.U(64.W))
//   // io.mem.req.valid := cmd.valid && doLoad && !busy && !stallResp
//   when(doLoad && !busy){
//       current:=address
//       stop:=address+(len*8)
//     }
//   when (io.mem.req.fire()) {
//     busy := true.B
//   }
//   io.mem.req.valid:=  (!done) && current=/=0.U && stop=/=0.U
//   // io.mem.req.bits.addr := address
//   io.mem.req.bits.tag := 0.U
//   io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
//   io.mem.req.bits.size := log2Ceil(8).U
//   io.mem.req.bits.signed := false.B
//   io.mem.req.bits.data := 0.U // we're not performing any stores...
//   io.mem.req.bits.phys := false.B
//   io.mem.req.bits.dprv := cmd.bits.status.dprv
  
//   //cmd.ready := !busy 
//   cmd.ready := !busy && !stallLoad && !stallResp
//   io.resp.valid := cmd.valid && doResp && !busy && done
//   io.resp.bits.rd := cmd.bits.inst.rd
//   io.resp.bits.data := result

//   io.busy := busy
//   io.interrupt := false.B
//   when(io.mem.req.valid){
//     switch(state){
//       is(reading){
//           result:=result+io.mem.resp.bits.data        
//           state:=sending
        
//       }
//       is(sending){
//         when(current>stop){
//           state:=idle
//         }.otherwise{
//             io.mem.req.bits.addr := current
//             state:=receiving
//           }
//        }
  
//     is(receiving){
//           when(io.mem.resp.valid){
//             current:=current+8.U
//             state:=reading
//           }
//     }
//     is(idle){
//       busy:=false.B
//       done:=true.B
//       }
// }
// }

// }



class WithMatrixRoCC extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new MatrixRoCC(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})
class WithMatrixRoCC2 extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new MatrixRoCC2(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})