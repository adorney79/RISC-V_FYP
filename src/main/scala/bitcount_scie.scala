package risc_v_fyp

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{DCacheLogicalTreeNode, LogicalModuleTree, RocketLogicalTreeNode, UTLBLogicalTreeNode}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import  freechips.rocketchip.scie._
import freechips.rocketchip.subsystem.TileCrossingParamsLike
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkParameters}
import chisel3.util.HasBlackBoxInline
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.LogicalTreeNode
import chisel3.experimental.IO


// The instruction simply loops over the value and increments a wire if the bit is 1.
class SCIEBitCount(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with scie_io {
  override val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
      |module SCIEBitCount #(parameter XLEN = 32) (
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
      |    result={XLEN{1'b0}};
      |    for (i=0; i<XLEN; i=i+1) begin
      |      if (rs1[i]==1'b1) begin
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

//defining a config for passing in the SCIE implementation

class WithSCIECores(n: Int = 1, overrideIdOffset: Option[Int] = None,scie:(Int)=>scie_io) extends Config((site, here, up) => {
case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    (0 until n).map { i =>
    MyTileAttachParams(
    tileParams=  SCIETileParams(
      hartId = i + idOffset,
      core   = RocketCoreParams(useSCIE = true,
      mulDiv = Some(MulDivParams(
        mulUnroll = 8,
        mulEarlyOut = true,
        divEarlyOut = true))),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        blockBytes = site(CacheBlockBytes))),
        imp=(b:SCIETile)=>new SCIEModuleImp(b,scie)
        ),
    crossingParams= RocketCrossingParams()
    )
    }++ prev
  }
})


case object SCIETilesKey extends Field[Seq[SCIETileParams]](Nil)

case class MyTileAttachParams(
  tileParams: SCIETileParams,
  crossingParams: RocketCrossingParams,
) extends CanAttachTile {
  type TileType = SCIETile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))

}

