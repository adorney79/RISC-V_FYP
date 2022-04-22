# RISC-V_FYP

## Setup
First follow the setup guide for Chipyard [here](https://chipyard.readthedocs.io/en/latest/Chipyard-Basics/Initial-Repo-Setup.html).
Next add this repo as a git submodule in the generators directory of Chipyard. Then you'll need to replace the top level build.sbt in Chipyard with
the one provided in the config_files folder. Alternatively just do a diff and add the necessary changes. Finally add the configs from config_files/RocketConfigs.scala to the file of the same name in Chipyard. You should now be able to generate these configs using the makefile in sims/verilator.
**Note:** The adjustments made in MyRocket.scala and AdaptedClasses.scala my not work with the latest branch of Rocket chip. Alternatively you can just take the SCIE implementations and paste them into the SCIE class already provided by Rocket core. You'll also need to enable SCIE for your default rocket config. Go to “generators/rocket-chip/src/main/scala/rocket/RocketCore.scala” and set **useSCIE** to true. Finally you can move the relevant benchmarks to your verilator directory in Chipyard. I've also included a shell script to compile the C files to bare metal .riscv binaries. Simply passs the file name (without the .c extension) to the script.  

## File structure

- /Benchmarks: Contains all C benchmarks for designs.
- /config_files: contains build.sbt for adding project to chipyard and the rocket chip configs for each design.
- /src/main/scala
  - AdaptedClasses.scala: Contains a number of classes from the Rocket chip hierarchy that are adapted to accept an SCIE implementation as input
  - MyRocket.scala: Contains a slightly adapted version of the Rocket class which accepts an SCIE implementation as input
  - RegexRocc.scala: Has the classes for a regular expression accelerator
  - bitcount_accel.scala: A basic RoCC class to test against the popcount instruction
  - bitcount_scie.scala: The SCIE implementation of a popcount instruction
  - matrix.scala: Contains all the matrix multiplication designs
  - sqrt.scala: The SCIE implementation of the bit manipulation in fast inverse square root.
