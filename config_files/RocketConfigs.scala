package chipyard

import freechips.rocketchip.config.{Config}
import freechips.rocketchip.diplomacy.{AsynchronousCrossing}
import risc_v_fyp.SCIEBitCount
import risc_v_fyp.SCIESQRT
import risc_v_fyp.PipelinedSQRT
// --------------
// Rocket Configs
// --------------
class BitCountRocket extends Config(
  new risc_v_fyp.WithBitcountRoCC ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)
class BitCountRocket2 extends Config(
  new risc_v_fyp.WithBitcountCores(1,None,((xlen:Int)=>new SCIEBitCount(xlen))) ++
  new chipyard.config.AbstractConfig
)
class SQRTRocket extends Config(
  new risc_v_fyp.WithBitcountCores(1,None,((xlen:Int)=>new SCIESQRT(xlen))) ++
  new chipyard.config.AbstractConfig
)
class MatrixRocket extends Config(
  new risc_v_fyp.WithMatrixRoCC ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)
class MatrixRocket2 extends Config(
  new risc_v_fyp.WithMatrixRoCC2 ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)
class MatrixRocket3 extends Config(
  new risc_v_fyp.WithMatrixRoCC3 ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)
class MatrixRocket4 extends Config(
  new risc_v_fyp.WithMatrixRoCC4 ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig
)
class PipelinedSQRTRocket extends Config(
  new risc_v_fyp.WithBitcountCores(1,None,((xlen:Int)=>new PipelinedSQRT(xlen))) ++
  new chipyard.config.AbstractConfig
)

