package risc_v_fyp

import freechips.rocketchip.rocket.RocketCoreParams
import freechips.rocketchip.rocket.ICacheParams
import freechips.rocketchip.rocket.DCacheParams
import freechips.rocketchip.rocket.BTBParams
import freechips.rocketchip.prci.ClockSinkParameters
import freechips.rocketchip.tile.InstantiableTileParams
import freechips.rocketchip.subsystem.TileCrossingParamsLike
import freechips.rocketchip.tile.LookupByHartIdImpl
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy.ClockCrossingType
import freechips.rocketchip.subsystem.RocketCrossingParams
import freechips.rocketchip.subsystem.CanAttachTile
import freechips.rocketchip.tile.PriorityMuxHartIdFromSeq
import freechips.rocketchip.rocket.MulDivParams
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.subsystem.TilesLocated
import freechips.rocketchip.subsystem.InSubsystem
import freechips.rocketchip.scie.SCIEPipelinedInterface
import chisel3.BlackBox
import freechips.rocketchip.scie.SCIE
import hardfloat._
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.LazyModule
import chisel3.util.PopCount

//instruction to perfrom the bit operations of fast inverse
class SCIESQRT(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with scie_io {
  override val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
         |module SCIESQRT #(parameter XLEN = 32) (
         |    input clock,
         |    input valid,
         |    input [${SCIE.iLen-1}:0] insn,
         |    input [XLEN-1:0] rs1,
         |    input [XLEN-1:0] rs2,
         |    output [XLEN-1:0] rd);
         |    wire [31:0] result =32'h5F3759DF-(rs1[31:0]>>1);
         |    assign rd=result;
         |
         |
         |endmodule
     """.stripMargin)
}

// a pipelined version of fast inverse.
// the compiler appears to optimise the original version to achieve this result anyway
class PipelinedSQRT(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with scie_io {
  override val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
         |module PipelinedSQRT #(parameter XLEN = 32) (
         |    input clock,
         |    input valid,
         |    input [${SCIE.iLen-1}:0] insn,
         |    input [XLEN-1:0] rs1,
         |    input [XLEN-1:0] rs2,
         |    output [XLEN-1:0] rd);
         |
         |  integer i;
         |  reg [31:0] shifter;
         |  reg [31:0] result;
         |
         |
         |  always @(posedge clock)
         |  begin
         |      shifter=rs1[31:0]>>1;
         |  end
         |
         |  /* In the second pipeline stage, compute the final result. */
         |  always @(*)
         |  begin
         |      result=32'h5F3759DF-shifter;
         |  end
         |
         |  /* Drive the output. */
         |  assign rd = result;
         |
         |
         |endmodule
     """.stripMargin)
}
case object SQRTTilesKey extends Field[Seq[SCIETileParams]](Nil)
case class SQRTAttachParams(
  tileParams: SCIETileParams,
  crossingParams: RocketCrossingParams,
) extends CanAttachTile {
  type TileType = SCIETile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))

}