package risc_v_fyp

import chisel3._
import freechips._
import chisel3.util.{HasBlackBoxResource}
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}
import chisel3.experimental.Param
import chisel3.util.BitPat
import chisel3.util.HasBlackBoxInline
import freechips.rocketchip.scie._
import freechips.rocketchip.rocket.Rocket
import chisel3.util.PopCount
import freechips.rocketchip.subsystem.WithNBigCores
import freechips.rocketchip.subsystem.TileCrossingParamsLike
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.rocket.RocketCoreParams
import freechips.rocketchip.rocket.MulDivParams
import freechips.rocketchip.rocket.DCacheParams
import freechips.rocketchip.rocket.ICacheParams
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.rocket.BTBParams
import freechips.rocketchip.prci.ClockSinkParameters

class BitRocket(tile:RocketTile)(implicit p: Parameters) extends Rocket(tile)(p){
  override val usingSCIE: Boolean = true
  override val rocketImpl.id_scie_decoder= if (!rocketParams.useSCIE) Wire(new SCIEDecoderInterface) else {
    val d = Module(new SCIEDecoder)
    assert(PopCount(d.io.unpipelined :: d.io.pipelined :: d.io.multicycle :: Nil) <= 1.U)
    d.io.insn := rocketImpl.id_raw_inst(0)
    d.io
  }
   override val rocketImpl.ex_scie_unpipelined_wdata = if (!rocketParams.useSCIE) 0.U else {
    val u = Module(new SCIEUnpipelined(xLen))
    u.io.insn := rocketImpl.ex_reg_inst
    u.io.rs1 := rocketImpl.ex_rs(0)
    u.io.rs2 := rocketImpl.ex_rs(1)
    u.io.rd
  }
  override val rocketImpl.mem_scie_pipelined_wdata = if (!rocketParams.useSCIE) 0.U else {
    val u = Module(new SCIEBitcount(xLen))
    u.io.clock := Module.clock
    u.io.valid := rocketImpl.ex_reg_valid && rocketImpl.ex_scie_pipelined
    u.io.insn := rocketImpl.ex_reg_inst
    u.io.rs1 := rocketImpl.ex_rs(0)
    u.io.rs2 := rocketImpl.ex_rs(1)
    u.io.rd
  }

}

class SCIEBitcount(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with HasBlackBoxInline {
  val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
      |module SCIEPipelined #(parameter XLEN = 32) (
      |    input clock,
      |    input valid,
      |    input [${SCIE.iLen-1}:0] insn,
      |    input [XLEN-1:0] rs1,
      |    input [XLEN-1:0] rs2,
      |    output [XLEN-1:0] rd);
      |
      |    integer i;
      |    reg [XLEN-1:0] result;
      |    always @(posedge clock)
      |    begin
      |    result={XLEN{1'b0};
      |    for (i=0; i<XLEN; i=i+1) begin
	    |      if (rs1[i]==1’b1) begin
		  |        result=result+1;
	    |        end
      |    end
      |  end
      |  assign rd=result;
      |
      |
      |endmodule
     """.stripMargin)
}

class BitCountModuleImp(outer: BitCountTile) extends RocketTileModuleImp(outer){
  override val core: BitRocket = Module(new BitRocket(outer)(outer.p))
}


class BitCountTile(rocketParams: RocketTileParams[BitCountTile],crossing: TileCrossingParamsLike,lookup: LookupByHartIdImpl)(implicit q:Parameters) 
extends RocketTile(rocketParams,crossing,lookup)(q){
  override lazy val module=new BitCountModuleImp(this)
}
// case class BitCountTileParams(
//     core: RocketCoreParams = RocketCoreParams(),
//     icache: Option[ICacheParams] = Some(ICacheParams()),
//     dcache: Option[DCacheParams] = Some(DCacheParams()),
//     btb: Option[BTBParams] = Some(BTBParams()),
//     dataScratchpadBytes: Int = 0,
//     name: Option[String] = Some("tile"),
//     hartId: Int = 0,
//     beuAddr: Option[BigInt] = None,
//     blockerCtrlAddr: Option[BigInt] = None,
//     clockSinkParams: ClockSinkParameters = ClockSinkParameters(),
//     boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
//     ) extends InstantiableTileParams[RocketTile] {
//   require(icache.isDefined)
//   require(dcache.isDefined)
//   def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): BitCountTile = {
//     new BitCountTile(this, crossing, lookup,p)
//   }
// }

// class WithBitcountCores(n: Int, overrideIdOffset: Option[Int] = None) extends WithNBigCores(n,overrideIdOffset){
// }
class WithBitcountCores (n: Int, overrideIdOffset: Option[Int] = None) extends Config((site, here, up) => {
   case RocketTilesKey => {
    val prev = up(RocketTilesKey, site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    val big = RocketTileParams[BitCountTile](
      (a:RocketTileParams[BitCountTile],b:TileCrossingParamsLike,c:LookupByHartIdImpl,q:Parameters)=>new BitCountTile(a,b,c)(q),
      core   = RocketCoreParams(mulDiv = Some(MulDivParams(
        mulUnroll = 8,
        mulEarlyOut = true,
        divEarlyOut = true))),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        blockBytes = site(CacheBlockBytes))))
    // List.tabulate(n)(i => big.copy(hartId = i + idOffset)) ++ prev
  }
})
