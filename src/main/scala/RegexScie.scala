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



class PipelinedRegex(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with scie_io {
  override val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
         |module PipelinedRegex #(parameter XLEN = 32) (
         |    input clock,
         |    input valid,
         |    input [${SCIE.iLen-1}:0] insn,
         |    input [XLEN-1:0] rs1,
         |    input [XLEN-1:0] rs2,
         |    output [XLEN-1:0] rd);
         |
         |  /* This example SCIE implementation provides the following instructions:
         |
         |     Major opcode custom-0:
         |     Funct3 = 2: AD.U8, compute absolute differences of packed uint8
         |       rd[7:0] = abs(rs1[7:0] - rs2[7:0])
         |       rd[15:8] = abs(rs1[15:8] - rs2[15:8])
         |       ...
         |       rd[XLEN-1:XLEN-8] = abs(rs1[XLEN-1:XLEN-8] - rs2[XLEN-1:XLEN-8])
         |
         |     Funct3 = 3: SAD.U8, compute sum of absolute differences of packed uint8
         |       tmp[7:0] = abs(rs1[7:0] - rs2[7:0])
         |       tmp[15:8] = abs(rs1[15:8] - rs2[15:8])
         |       ...
         |       tmp[XLEN-1:XLEN-8] = abs(rs1[XLEN-1:XLEN-8] - rs2[XLEN-1:XLEN-8])
         |
         |       rd = tmp[7:0] + tmp[15:8] + ... + tmp[XLEN-1:XLEN-8]
         |  */
         |
         |  integer i;
         |  reg [31:0] stage;
         |  reg [31:0] substage;
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
case object RegexTilesKey extends Field[Seq[BitCountTileParams]](Nil)
case class RegexAttachParams(
  tileParams: BitCountTileParams,
  crossingParams: RocketCrossingParams,
) extends CanAttachTile {
  type TileType = BitCountTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))

}

