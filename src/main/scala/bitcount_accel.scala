package risc_v_fyp
import freechips._
import Chisel._
import chisel3.util.{HasBlackBoxResource}
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}
import chisel3.experimental.Param
import chisel3.chiselTypeOf
import Chisel.ImplicitConversions._


class BitcountRoCC(opcodes:OpcodeSet)
(implicit p: Parameters) extends LazyRoCC(opcodes){
    override lazy val module=new BitcountModule(this)
}
class BitcountModule(outer: BitcountRoCC)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
    val cmd=Queue(io.cmd)
    cmd.ready:=cmd.valid
    io.resp.valid:=cmd.valid

    io.resp.bits.rd:=cmd.bits.inst.rd
    io.resp.bits.data:=PopCount(cmd.bits.rs1)
}



class WithBitcountRoCC extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new BitcountRoCC(OpcodeSet.custom0)(p))
            myrocc
    }
  )
})



class DemoModule(outer: DemoRoCC)(implicit p: Parameters)
extends LazyRoCCModuleImp(outer) with HasCoreParameters{
    //user defined values
    val cmd=Queue(io.cmd)
    val funct = cmd.bits.inst.funct
    val read = funct === 1.U
    val add = funct === 2.U
    val revert=funct===0.U
    val address=RegInit(0.U(40.W))
    val result=RegInit(0.U(64.W))
    val busy=RegInit(false.B)
    //memory related values
    io.mem.req.valid:=busy && address =/=0.U
    io.mem.req.bits.addr:=address
    io.mem.req.bits.tag := 0.U
    io.mem.req.bits.cmd := M_XRD // perform a load 
    io.mem.req.bits.size := log2Ceil(8).U //grabbing 8 addresses (64 bits)
    io.mem.req.bits.signed := false.B
    io.mem.req.bits.data := 0.U
    io.mem.req.bits.phys := false.B
    io.mem.req.bits.dprv := 3.U
    //input/output values
    io.busy:=busy
    cmd.ready:=(!busy)
    io.resp.valid:=cmd.valid && read && !busy
    io.resp.bits.rd:=cmd.bits.inst.rd
    io.resp.bits.data:=result
    //begins task
    when(add){
      address:=cmd.bits.rs1
      busy:=true.B
    }
    //resets accelerator
    when(revert && !busy){
      busy:=false
      address:=0.U
    }
    //retrieves value from cache and increments
    when(address=/= 0.U && io.mem.resp.valid){
        result:=io.mem.resp.bits.data +1.U
        address:=0.U

        busy:=false.B
    }
}



class WithDemoRoCC extends Config ((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
         val myrocc=LazyModule.apply(new DemoRoCC(OpcodeSet.custom0)(p))
            myrocc
    }
  )
})
class DemoRoCC(opcodes:OpcodeSet)
(implicit p: Parameters) extends LazyRoCC(opcodes){
    override lazy val module=new DemoModule(this)
}