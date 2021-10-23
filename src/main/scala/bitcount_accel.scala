package risc_v_fyp
import chisel3._
import freechips._
import Chisel._
import chisel3.util.{HasBlackBoxResource}
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}
import chisel3.experimental.Param


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