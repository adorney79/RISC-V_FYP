package risc_v_fyp.tests
// import Chisel._
// import Chisel.iotesters
// import chisel3.iotesters.PeekPokeTester
// import risc_v_fyp.WithMatrixRoCC
// import risc_v_fyp.MatrixModule
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.must.Matchers
// import risc_v_fyp.MatrixRoCC
// import freechips.rocketchip.tile.OpcodeSet
// import chipsalliance.rocketchip.config.Parameters
// class MatrixTest(m:MatrixRocket) extends PeekPokeTester(m){
  
// }

// class MatrixSpec extends AnyFlatSpec with Matchers{
//     behavior of "MatrixModule"
    
//     it should "compute stuff" in {
//         Driver.execute(Array(),()=>new MatrixModule(new MatrixRoCC(OpcodeSet.custom0,1,false)())){
//             m=>
//                 new MatrixTest(m)
//         }should be(true)
//     }
// }
// import Chisel._
// import freechips.rocketchip.config
// import freechips.rocketchip.diplomacy
// import freechips.rocketchip.unittest
// import freechips.rocketchip.unittest.HasUnitTestIO
// import chipsalliance.rocketchip
// import freechips.rocketchip.tilelink.TLRAM
// import risc_v_fyp.MatrixRoCC
// import freechips.rocketchip.tile.OpcodeSet

// class RoccUnitTester()(implicit p: rocketchip.config.Parameters)  extends diplomacy.LazyModule {
//   val ram  = diplomacy.LazyModule(new TLRAM(diplomacy.AddressSet(0x0, 0x3ff)))
//   val legacy = diplomacy.LazyModule(new TLLegacy)
//   ram.node := legacy.node

// //   val coreParams = p.alterPartial {
// //     case TLCacheEdge => legacy.node.edgesOut(0)
// //     //...other params you're supplying
// //   }

//   lazy val module = new diplomacy.LazyModuleImp(this) with HasUnitTestIO {

//     val rocc = diplomacy.LazyModule.apply(new MatrixRoCC(OpcodeSet.custom0,1,false)(p))
//     // feed rocc input ports
//     // tie off unused rocc output ports
//     legacy.module.io.legacy <> rocc.io.autl

//     io.finished := Bool(false) // or replace with however you signal this
// }
// }

// class RoccUnitTest()(implicit p: rocketchip.config.Parameters) extends unittest.UnitTest(timeout = 500000) {
//   io.finished := Module(diplomacy.LazyModule(new RoccUnitTester()(p)).module).io.finished
// }