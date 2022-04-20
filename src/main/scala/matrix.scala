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

// this file contains all the matrix multiplication designs
// all these designs use 64 bit numbers but could easily be adapted to accept any size number
// through an additional setup instruction. Floating point operations could also be added by simply
// using the FPU interface.
class MatrixRoCC(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new MatrixModule(this)
}
class MatrixRoCC2(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new MatrixModule2(this)
}
class MatrixRoCC3(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new MatrixModule3(this)
}
class MatrixRoCC4(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new MatrixModule4(this)
}
class SumReduce(opcodes:OpcodeSet,nPTWPorts:Int,usesFPU:Boolean)
(implicit p: Parameters) extends LazyRoCC(opcodes,nPTWPorts,usesFPU){
    override lazy val module=new SumReduceModule(this)
}

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
class WithMatrixRoCC3 extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new MatrixRoCC3(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})
class WithMatrixRoCC4 extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new MatrixRoCC4(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})
class WithSumReduce extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new SumReduce(OpcodeSet.custom0,1,false)(p))
            myrocc
    }
  )
})
// a design with limited resources
class MatrixModule(outer: MatrixRoCC)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
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
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load 
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U 
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
  cmd.ready := !busy && !stallLoad && !stallResp
  io.resp.valid := cmd.valid && doResp && !busy && done && io.mem.resp.bits.data===0.U
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := current

  io.busy := busy
  io.interrupt := false.B
  switch(state){
    //wait for result address to start
      is(idle){
        when(z_addr=/=0.U){
          state:=read_row
          busy:=true.B
        }
      }
      // unless we're finished, request the next value in the current row
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
      //stores value and prepares to request column
      is(recv_row){
        when(io.mem.resp.valid){
            row_val:=io.mem.resp.bits.data
            state:=read_col
        }
      }
      //requests next value from current column
        is(read_col){
            io.mem.req.bits.cmd:=M_XRD
            io.mem.req.bits.addr:=(y_len*offset)*row_idx+current_col*offset+y_addr
            when(io.mem.req.valid && io.mem.req.ready){
                state:=recv_col
            }
        }
        //stores result and prepares for calculation
        is(recv_col){
          when(io.mem.resp.valid){
            row_val:=row_val*io.mem.resp.bits.data
            row_idx:=row_idx+1.U
            state:=calculate
        }
        }
        //if we have the results for a row/column pair we proceed to the write state
        // otherwise we need more values and return to getting the next row value
        is(calculate){
          current:=current+row_val
            when(row_idx===x_len){
              state:=write
              row_idx:=0.U
            }.otherwise{
              state:=read_row
            }
        }
        // we write to a specified address and increment either the row or column
        // if we're at the final column then we reset the column pointer and increment to the next row.
        // otherwise we simple proceed to the next column
        is(write){
          io.mem.req.bits.cmd:=M_XWR //used to specify write
          io.mem.req.bits.data:=current
          io.mem.req.bits.addr:=z_addr+x_len*offset*current_row+current_col*offset
          when(io.mem.resp.valid){
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
    }
// memory abundant design
class MatrixModule2(outer: MatrixRoCC2)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
  val rows=Reg(Vec(64, UInt(64.W)))

  val cols=Reg(Vec(4096, UInt(64.W)))
  val blocksize=64.U
  val stored=RegInit(false.B)
  val counter=RegInit(0.U(7.W))
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
  val offset=8.U
  val idle::read_row::recv_row::read_col::recv_col::calculate::write::Nil=Enum(7)
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
  val overflow=RegInit(0.U(64.W))


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
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U 
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
  cmd.ready := !busy && !stallLoad && !stallResp
  io.resp.valid := cmd.valid && doResp && !busy && done && io.mem.resp.bits.data===0.U
  io.resp.bits.rd := cmd.bits.inst.rd
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
// unless we're finished, get the next row and store it.
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
 
          rows(resp_tag):=io.mem.resp.bits.data
      
          when(!io.mem.req.valid && !io.mem.resp.valid){
            //if the second matrix is stored already we skip to calculate
            state:=Mux(stored,calculate,read_col)
            counter:=0.U
            row_idx:=0.U
            
          }
        }

      }
//simply reads in entire second matrix into memory
        is(read_col){
            io.mem.req.valid:=  row_idx=/=y_height && counter=/=63.U
            io.mem.req.bits.addr:=(y_len*offset)*row_idx+current_col*offset+y_addr
            io.mem.req.bits.tag:=counter
            when(io.mem.req.valid && io.mem.req.ready){
              counter:=counter+1.U
              when(current_col===y_len-1.U){
                  row_idx:=row_idx+1.U
                  current_col:=0.U
                }.otherwise{
                  current_col:=current_col+1.U
                }

            }
            //accounting for overflow in tag wire
            when(counter===63.U){
                overflow:=overflow+1.U
                when(!io.mem.resp.valid){
                  counter:=0.U
                }
              }
          cols(resp_tag+(overflow*63.U)):=io.mem.resp.bits.data
          when(!io.mem.req.valid && !io.mem.resp.valid && counter=/=63.U){
            stored:=true.B
            state:=calculate
            current_col:=0.U
            row_idx:=0.U
            counter:=0.U
            overflow:=0.U

          }

        }
      // same as previous design but continues calculating and writing until we finish with the row
        is(calculate){
          io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height

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
            current:=current+(rows(row_idx)*cols(current_col+(row_idx*y_len)))
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

  //transpose design
class MatrixModule3(outer: MatrixRoCC3)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{

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
  val offset=8.U
  val idle::read_row::recv_row::read_col::recv_col::calculate::write::read_copy::write_copy::wait_write_copy::wait_read_copy::Nil=Enum(11)
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
  val copy_addr=RegInit(0.U(64.W))

  val write_val=RegInit(0.U(64.W))
  val current_row=RegInit(0.U(64.W))
  val current_col=RegInit(0.U(64.W))
  val row_idx=RegInit(0.U(64.W))
  val row_val=RegInit(0.U(64.W))
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
  when(doOut && !busy && !done){
    z_addr:=cmd.bits.rs1
    copy_addr:=cmd.bits.rs2
  }

  when (io.mem.req.fire()) {
    busy := true.B
  }
  io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write || state===read_copy || state===write_copy)&& current_row=/=x_height
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load 
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U 
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
  cmd.ready := !busy && !stallLoad && !stallResp
  io.resp.valid := cmd.valid && doResp && !busy && done && io.mem.resp.bits.data===0.U
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := current

  io.busy := busy
  io.interrupt := false.B
  switch(state){
      is(idle){
        when(z_addr=/=0.U){
          state:=read_copy    
          busy:=true.B
        }
      }
      // this section can be a bit tricky to decipher.
      // essentially it goes row by row in the second matrix, writing each value to a column address
      // going row by row instead of column by column ensures more cache hits which will speed up this process
      is(read_copy){
        when(io.mem.req.valid && io.mem.req.ready){
          io.mem.req.bits.addr:=(y_len*offset)*row_idx+current_col*offset+y_addr
          state:=wait_read_copy
        }
      }
      is(wait_read_copy){
        when(io.mem.resp.valid){
          state:=write_copy
        }
      }
      is(write_copy){
        io.mem.req.bits.addr:=(y_len*offset)*current_col+row_idx*offset+copy_addr
          io.mem.req.bits.data:=io.mem.resp.bits.data
          io.mem.req.bits.tag:=1.U
          io.mem.req.bits.cmd:=M_XWR
        when(io.mem.resp.valid && io.mem.resp.bits.tag===1.U){
          state:=wait_write_copy
        }
      }
      is(wait_write_copy){
        io.mem.req.bits.tag:=0.U
          io.mem.req.bits.cmd := M_XRD
          when(row_idx===y_height-1.U && current_col===x_len-1.U){
            current_col:=0.U
            row_idx:=0.U
            io.mem.req.bits.addr:=x_addr
            state:=read_row
          }.elsewhen(row_idx===y_len-1.U){
            row_idx:=0.U
            current_col:=current_col+1.U
            state:=read_copy
          }.otherwise{
            row_idx:=row_idx+1.U
            state:=read_copy
          }
        
        

      }
      // from here onwards is the same as design 1 but it reads from rows in each matrix instead of
      // rows and columns
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
            io.mem.req.bits.addr:=(y_len*offset)*current_col+row_idx*offset+copy_addr
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
            }.otherwise{
              state:=read_row
            }
        }
        is(write){
          io.mem.req.bits.cmd:=M_XWR
          io.mem.req.bits.data:=current
          io.mem.req.bits.addr:=z_addr+x_len*offset*current_row+current_col*offset
          when(io.mem.resp.valid){
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
    }
//transpose and use 2 vector registers
class MatrixModule4(outer: MatrixRoCC4)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
  val rows=Reg(Vec(64, UInt(64.W)))

  val cols=Reg(Vec(64, UInt(64.W)))
  val blocksize=64.U
  val counter=RegInit(0.U(7.W))
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
  val offset=8.U
  val idle::read_row::recv_row::read_col::recv_col::calculate::write::read_copy::write_copy::wait_write_copy::wait_read_copy::Nil=Enum(11)
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
  val overflow=RegInit(0.U(64.W))
  val copy_addr=RegInit(0.U(64.W))


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
  when(doOut && !busy && !done){
    z_addr:=cmd.bits.rs1
    copy_addr:=cmd.bits.rs2
  }

  when (io.mem.req.fire()) {
    busy := true.B
  }
   io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write || state===read_copy || state===write_copy)&& current_row=/=x_height
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load 
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U 
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := cmd.bits.status.dprv
  
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
          state:=read_copy    
          busy:=true.B
        }
      }
      //same state machine as transpose model
      is(read_copy){
        when(io.mem.req.valid && io.mem.req.ready){
          io.mem.req.bits.addr:=(y_len*offset)*row_idx+current_col*offset+y_addr
          state:=wait_read_copy
        }
      }
      is(wait_read_copy){
        when(io.mem.resp.valid){
          state:=write_copy
        }
      }
      is(write_copy){
        io.mem.req.bits.addr:=(y_len*offset)*current_col+row_idx*offset+copy_addr
          io.mem.req.bits.data:=io.mem.resp.bits.data
          io.mem.req.bits.tag:=1.U
          io.mem.req.bits.cmd:=M_XWR
        when(io.mem.resp.valid && io.mem.resp.bits.tag===1.U){
          state:=wait_write_copy
        }
      }
      is(wait_write_copy){
        io.mem.req.bits.tag:=0.U
          io.mem.req.bits.cmd := M_XRD
          when(row_idx===y_height-1.U && current_col===x_len-1.U){
            current_col:=0.U
            row_idx:=0.U
            io.mem.req.bits.addr:=x_addr
            state:=read_row
          }.elsewhen(row_idx===y_len-1.U){
            row_idx:=0.U
            current_col:=current_col+1.U
            state:=read_copy
          }.otherwise{
            row_idx:=row_idx+1.U
            state:=read_copy
          }
      }
// unless we're finished, read a row into a vector register
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

          rows(resp_tag):=io.mem.resp.bits.data
      
          when(!io.mem.req.valid && !io.mem.resp.valid){
            state:=read_col
            counter:=0.U
            row_idx:=0.U
            
          }
        }

      }
// next read a column into the other vector register
        is(read_col){
            io.mem.req.valid:=  row_idx=/=y_height
            io.mem.req.bits.addr:=(y_len*offset)*current_col+row_idx*offset+copy_addr
            io.mem.req.bits.tag:=row_idx
            when(io.mem.req.valid && io.mem.req.ready){
              row_idx:=row_idx+1.U

            }
          cols(resp_tag):=io.mem.resp.bits.data
          when(!io.mem.req.valid && !io.mem.resp.valid ){
            state:=calculate
            row_idx:=0.U
            overflow:=0.U

          }

        }
        // caclulates dot product of row and column
        is(calculate){
          io.mem.req.valid:=  (!done) && (state===read_row||state===read_col||state===write)&& current_row=/=x_height

          when(row_idx===x_len){
            state:=write
            row_idx:=0.U
          }.otherwise{
            current:=current+(rows(row_idx)*cols(row_idx))
            row_idx:=row_idx+1.U
          }
        }
        // writes out to memory and sets up to retrieve the next column provided we
        // aren't at the last column. This means that every row is stored once but every column
        // must be loaded in again for however many rows there are.
        is(write){
          io.mem.req.bits.cmd:=M_XWR
          io.mem.req.bits.data:=current
          io.mem.req.bits.addr:=z_addr+x_len*offset*current_row+current_col*offset
          when(io.mem.resp.valid){
            current_col:=current_col+1.U
            current:=0.U
            when(current_col===y_len-1.U){
              io.mem.req.bits.cmd:=M_XRD
              current_col:=0.U
              current_row:=current_row+1.U
              state:=read_row
            }.otherwise{
              state:=read_col
            }
          }  
        }

      }
    }

// basic class used for early development that sum reduces an array
class SumReduceModule(outer: SumReduce)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{

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
  val sending :: reading  ::ready::idle::receiving::inter::Nil = Enum(6)
  val current = RegInit(0.U(64.W)) // value index
  val state = RegInit(sending)
  val stop=RegInit(0.U(64.W))
  when(doLoad && !busy){
      current:=address
      stop:=address+(len*8)
    }
  when (io.mem.req.fire) {
    busy := true.B
  }
  io.mem.req.valid:=  (!done) && current=/=0.U && stop=/=0.U && (state===receiving)
  io.mem.req.bits.tag := 0.U
  io.mem.req.bits.cmd := M_XRD // perform a load 
  io.mem.req.bits.size := log2Ceil(8).U
  io.mem.req.bits.signed := false.B
  io.mem.req.bits.data := 0.U 
  io.mem.req.bits.phys := false.B
  io.mem.req.bits.dprv := 3.U
  
  cmd.ready := !busy && !stallLoad && !stallResp
  io.resp.valid := cmd.valid && doResp && !busy && done
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := result

  io.busy := busy
  io.interrupt := false.B
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
            state:=inter
          }
       }
    is(inter){
      when(io.mem.req.bits.addr=/=0.U){
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



