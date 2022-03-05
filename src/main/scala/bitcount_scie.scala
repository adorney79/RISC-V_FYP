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
// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.
trait scie_io extends HasBlackBoxInline{
    val io:SCIEPipelinedInterface

}
// class BitRocket(tile:RocketTile)(implicit p: Parameters) extends Rocket(tile)(p){
//   override val usingSCIE: Boolean = true
//    override val rocketImpl.ex_scie_unpipelined_wdata = if (!rocketParams.useSCIE) 0.U else {
//     val u = Module(new SCIEUnpipelined(xLen))
//     u.io.insn := rocketImpl.ex_reg_inst
//     u.io.rs1 := rocketImpl.ex_rs(0)
//     u.io.rs2 := rocketImpl.ex_rs(1)
//     u.io.rd
//   }
//   override val rocketImpl.mem_scie_pipelined_wdata = if (!rocketParams.useSCIE) 0.U else {
//     val u = Module(new SCIEBitcount(xLen))
//     u.io.clock := Module.clock
//     u.io.valid := rocketImpl.ex_reg_valid && rocketImpl.ex_scie_pipelined
//     u.io.insn := rocketImpl.ex_reg_inst
//     u.io.rs1 := rocketImpl.ex_rs(0)
//     u.io.rs2 := rocketImpl.ex_rs(1)
//     u.io.rd
//   }

// }

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

class BitCountModuleImp(outer: BitCountTile,scieimp:(Int)=>scie_io) extends BaseTileModuleImp(outer)
    with HasMyFpuOpt
    with HasMyRoCCModule
    with HasICacheFrontendModule {
  Annotated.params(this, outer.rocketParams)

  val core = Module(new BitRocket(outer,scieimp)(outer.p))

  // Report unrecoverable error conditions; for now the only cause is cache ECC errors
  outer.reportHalt(List(outer.dcache.module.io.errors))

  // Report when the tile has ceased to retire instructions; for now the only cause is clock gating
  outer.reportCease(outer.rocketParams.core.clockGate.option(
    !outer.dcache.module.io.cpu.clock_enabled &&
    !outer.frontend.module.io.cpu.clock_enabled &&
    !ptw.io.dpath.clock_enabled &&
    core.io.cease))

  outer.reportWFI(Some(core.io.wfi))

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  outer.bus_error_unit.foreach { beu =>
    core.io.interrupts.buserror.get := beu.module.io.interrupt
    beu.module.io.errors.dcache := outer.dcache.module.io.errors
    beu.module.io.errors.icache := outer.frontend.module.io.errors
  }

  core.io.interrupts.nmi.foreach { nmi => nmi := outer.nmiSinkNode.bundle }

  // Pass through various external constants and reports that were bundle-bridged into the tile
  outer.traceSourceNode.bundle <> core.io.trace
  core.io.traceStall := outer.traceAuxSinkNode.bundle.stall
  outer.bpwatchSourceNode.bundle <> core.io.bpwatch
  core.io.hartid := outer.hartIdSinkNode.bundle
  require(core.io.hartid.getWidth >= outer.hartIdSinkNode.bundle.getWidth,
    s"core hartid wire (${core.io.hartid.getWidth}b) truncates external hartid wire (${outer.hartIdSinkNode.bundle.getWidth}b)")

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.imem
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu => core.io.fpu <> fpu.io }
  core.io.ptw <> ptw.io.dpath

  // Connect the coprocessor interfaces
  if (outer.roccs.size > 0) {
    cmdRouter.get.io.in <> core.io.rocc.cmd
    outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
    core.io.rocc.resp <> respArb.get.io.out
    core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_ || _))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_ || _)
  }

  // Rocket has higher priority to DTIM than other TileLink clients
  outer.dtim_adapter.foreach { lm => dcachePorts += lm.module.io.dmem }

  // TODO eliminate this redundancy
  val h = dcachePorts.size
  val c = core.dcacheArbPorts
  val o = outer.nDCachePorts
  require(h == c, s"port list size was $h, core expected $c")
  require(h == o, s"port list size was $h, outer counted $o")
  // TODO figure out how to move the below into their respective mix-ins
  dcacheArb.io.requestor <> dcachePorts
  ptw.io.requestor <> ptwPorts
}

// class BitCountTile(rocketParams: RocketTileParams,crossing: TileCrossingParamsLike,lookup: LookupByHartIdImpl)(implicit q:Parameters) 
// extends RocketTile(rocketParams,crossing,lookup)(q){
//   override lazy val module=new BitCountModuleImp(this,new BitRocket(this)(this.p))
// }
case class BitCountTileParams(
    core: RocketCoreParams = RocketCoreParams(useSCIE = true),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    name: Option[String] = Some("tile"),
    hartId: Int = 0,
    beuAddr: Option[BigInt] = None,
    blockerCtrlAddr: Option[BigInt] = None,
    clockSinkParams: ClockSinkParameters = ClockSinkParameters(),
    boundaryBuffers: Boolean = false ,// if synthesized with hierarchical PnR, cut feed-throughs?
    imp: (BitCountTile)=>BitCountModuleImp,
    ) extends InstantiableTileParams[BitCountTile] {
  require(icache.isDefined)
  require(dcache.isDefined)
  def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): BitCountTile = {
    new BitCountTile(this, crossing, lookup,(imp))
  }
}




class BitCountTile private(
      val rocketParams: BitCountTileParams,
      crossing: ClockCrossingType,
      lookup: LookupByHartIdImpl,
      q: Parameters,
      imp: (BitCountTile)=>BitCountModuleImp)
    extends BaseTile(rocketParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with HasHellaCache
    with HasICacheFrontend
{
  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: BitCountTileParams, crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl,imp:(BitCountTile)=>BitCountModuleImp)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p,imp)

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  override val logicalTreeNode = new MyLogicalTreeNode(this, p(XLen), pgLevels)

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map { s =>
    LazyModule(new ScratchpadSlavePort(AddressSet.misaligned(s, d.dataScratchpadBytes), lazyCoreParamsView.coreDataBytes, tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO))
  }}
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, lm.node.portParams.head.beatBytes))

  val bus_error_unit = rocketParams.beuAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a), logicalTreeNode))
    intOutwardNode := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = frontend.icache.itimProperty.toSeq.flatMap(p => Map("sifive,itim" -> p))

  val beuProperty = bus_error_unit.map(d => Map(
          "sifive,buserror" -> d.device.asProperty)).getOrElse(Nil)

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("sifive,rocket0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty
                  ++ tileProperties ++ dtimProperty ++ itimProperty ++ beuProperty)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(staticIdForMetadataUseOnly))
  }

  override lazy val module = imp(this)

  override def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!rocketParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
    case _ => TLBuffer(BufferParams.none)
  }

  override def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!rocketParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
    case _ => TLBuffer(BufferParams.none)
  }

  val dCacheLogicalTreeNode = new DCacheLogicalTreeNode(dcache, dtim_adapter.map(_.device), rocketParams.dcache.get)
  LogicalModuleTree.add(logicalTreeNode, iCacheLogicalTreeNode)
  LogicalModuleTree.add(logicalTreeNode, dCacheLogicalTreeNode)

  if (rocketParams.core.useVM) {
    val utlbLogicalTreeNode = new UTLBLogicalTreeNode(rocketParams.core, utlbOMSRAMs)
    LogicalModuleTree.add(logicalTreeNode, utlbLogicalTreeNode)
  }
}

trait HasMyRoCCModule extends CanHavePTWModule
    with HasCoreParameters { this: BitCountModuleImp with HasMyFpuOpt =>

  val (respArb, cmdRouter) = if(outer.roccs.nonEmpty) {
    val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
    val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
    outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
      rocc.module.io.ptw ++=: ptwPorts
      rocc.module.io.cmd <> cmdRouter.io.out(i)
      val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
      dcIF.io.requestor <> rocc.module.io.mem
      dcachePorts += dcIF.io.cache
      respArb.io.in(i) <> Queue(rocc.module.io.resp)
    }

    fpuOpt foreach { fpu =>
      val nFPUPorts = outer.roccs.count(_.usesFPU)
      if (usingFPU && nFPUPorts > 0) {
        val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
        val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
        fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
        fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
          case (rocc, arb) => rocc.fpu_resp <> arb
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      } else {
        fpu.io.cp_req.valid := false.B
        fpu.io.cp_resp.ready := false.B
      }
    }
    (Some(respArb), Some(cmdRouter))
  } else {
    (None, None)
  }
}
trait HasMyFpuOpt { this: BitCountModuleImp =>
  val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
}

class MyLogicalTreeNode(
  tile: BitCountTile,
  XLen: Int,
  PgLevels: Int
) extends LogicalTreeNode(() => Some(tile.cpuDevice)) {

  def getOMInterruptTargets(): Seq[OMInterruptTarget] = {
    Seq(OMInterruptTarget(
      hartId = tile.rocketParams.hartId,
      modes = OMModes.getModes(tile.rocketParams.core.hasSupervisorMode, tile.rocketParams.core.useHypervisor)
    ))
  }

  override def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    val rocketParams = tile.rocketParams
    val coreParams = rocketParams.core

    // Expect that one of the components passed in is the DCache/DTIM.
    val omDCache = components.collectFirst { case x: OMDCache => x }.get

    // Expect that one of the components passed in is the ICache.
    val omICache = components.collectFirst { case x: OMICache => x }.get

    // Expect that one of the components passed in is the UTLB.
    val omUTLB = components.collectFirst { case x: OMUTLB => x }

    val omBusError = components.collectFirst { case x: OMBusError => x }

    Seq(OMRocketCore(
      isa = MyOMISA.rocketISA(tile, XLen, PgLevels),
      mulDiv =  coreParams.mulDiv.map{ md => OMMulDiv.makeOMI(md, XLen)},
      fpu = coreParams.fpu.map{f => OMFPU(fLen = f.fLen, minFLen = f.minFLen)},
      performanceMonitor = PerformanceMonitor.perfmon(coreParams),
      pmp = OMPMP.pmp(coreParams),
      documentationName = rocketParams.name.getOrElse("rocket"),
      hartIds = Seq(rocketParams.hartId),
      hasVectoredInterrupts = true,
      interruptLatency = 4,
      nLocalInterrupts = coreParams.nLocalInterrupts,
      rnmiPresent = coreParams.useNMI,
      unmiPresent = coreParams.useNMI,
      nBreakpoints = coreParams.nBreakpoints,
      mcontextWidth = coreParams.mcontextWidth,
      scontextWidth = coreParams.scontextWidth,
      branchPredictor = rocketParams.btb.map(OMBTB.makeOMI),
      dcache = Some(omDCache),
      icache = Some(omICache),
      busErrorUnit = omBusError,
      hasClockGate = coreParams.clockGate,
      hasSCIE = coreParams.useSCIE,
      vmPresent = coreParams.useVM,
      utlb = omUTLB
    ))
  }
}

object MyOMISA {
  def rocketISA(tile: BitCountTile, xLen: Int, pgLevels: Int): OMISA = {
    val coreParams = tile.rocketParams.core

    val baseInstructionSet = xLen match {
      case 32 => if (coreParams.useRVE) RV32E else RV32I
      case 64 => if (coreParams.useRVE) RV64E else RV64I
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
    }

    val customExtensions = {
      if (coreParams.haveCFlush) List (Xsifivecflushdlone(full = true, line = tile.dcache.canSupportCFlushLine)) else Nil
    }

    val isaExtSpec = ISAExtensions.specVersion _

    val baseSpec = BaseExtensions.specVersion _

    val baseISAVersion = baseInstructionSet match {
      case RV32E => "1.9"
      case RV32I => "2.0"
      case RV64E => "1.9"
      case RV64I => "2.0"
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid baseISAVersion: $baseInstructionSet")
    }

    val addressTranslationModes = xLen match {
      case _ if !coreParams.useVM => Bare
      case 32 if (pgLevels == 2) => Sv32
      case 64 if (pgLevels == 3) => Sv39
      case 64 if (pgLevels == 4) => Sv48
      case 64 if (pgLevels == 5) => Sv57
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen/PgLevels combination: $xLen/$pgLevels")
    }

    OMISA(
      xLen = xLen,
      baseSpecification = baseSpec(baseInstructionSet, baseISAVersion),
      base = baseInstructionSet,
      m = coreParams.mulDiv.map(x => isaExtSpec(M, "2.0")),
      a = coreParams.useAtomics.option(isaExtSpec(A, "2.0")),
      f = coreParams.fpu.map(x => isaExtSpec(F, "2.0")),
      d = coreParams.fpu.filter(_.fLen > 32).map(x => isaExtSpec(D, "2.0")),
      c = coreParams.useCompressed.option(isaExtSpec(C, "2.0")),
      u = (coreParams.hasSupervisorMode || coreParams.useUser).option(isaExtSpec(U, "1.10")),
      s = coreParams.hasSupervisorMode.option(isaExtSpec(S, "1.10")),
      h = coreParams.useHypervisor.option(isaExtSpec(H, "0.6")),
      addressTranslationModes = Seq(addressTranslationModes),
      customExtensions = customExtensions
    )
  }
}
class WithBitcountCores(n: Int = 1, overrideIdOffset: Option[Int] = None,scie:(Int)=>scie_io) extends Config((site, here, up) => {
case TilesLocated(InSubsystem) => {
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    (0 until n).map { i =>
    MyTileAttachParams(
    tileParams=  BitCountTileParams(
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
        imp=(b:BitCountTile)=>new BitCountModuleImp(b,scie)
        ),
    crossingParams= RocketCrossingParams()
    )
    }++ prev
  }
})


case object BitCountTilesKey extends Field[Seq[BitCountTileParams]](Nil)

case class MyTileAttachParams(
  tileParams: BitCountTileParams,
  crossingParams: RocketCrossingParams,
) extends CanAttachTile {
  type TileType = BitCountTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))

}

