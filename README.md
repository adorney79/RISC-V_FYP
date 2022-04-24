# RISC-V_FYP

## Setup
Setting this repo up will require some prior knowledge of Chipyard. Chipyard is also an extremely large repo so I'd advise using a system with roughly
50GB free space and at least 8GB of RAM.
First follow the setup guide for Chipyard [here](https://chipyard.readthedocs.io/en/latest/Chipyard-Basics/Initial-Repo-Setup.html).
Next add this repo as a git submodule in the generators directory of Chipyard. There's some useful info on how to do so [here](https://chipyard.readthedocs.io/en/latest/Customization/Custom-Chisel.html)

You'll need to replace the top level build.sbt in Chipyard with
the one provided in the config_files folder. Alternatively just do a diff and add the necessary changes. Finally add the configs from config_files/RocketConfigs.scala to the file of the same name in Chipyard. You should now be able to generate these configs using the makefile in sims/verilator.
**Note:** The adjustments made in MyRocket.scala and AdaptedClasses.scala may not work with the latest branch of Rocket chip. Alternatively you can just take the SCIE implementations and paste them into the SCIE class already provided by Rocket core. You'll also need to enable SCIE for your default rocket config. Go to “generators/rocket-chip/src/main/scala/rocket/RocketCore.scala” and set **useSCIE** to true. Finally you can move the relevant benchmarks to your verilator directory in Chipyard. I've also included a shell script to compile the C files to bare metal .riscv binaries. Simply pass the file name (without the .c extension) to the script.  

## File structure

- /Benchmarks: Contains all C benchmarks for designs.
- /config_files: contains build.sbt for adding project to chipyard and the rocket chip configs for each design.
- /src/main/scala
  - AdaptedClasses.scala: Contains a number of classes from the Rocket chip hierarchy that are adapted to accept an SCIE implementation as input (mostly code from rocket chip and not my own)
  - MyRocket.scala: Contains a slightly adapted version of the Rocket class which accepts an SCIE implementation as input (mostly code from rocket chip and not my own)
  - RegexRocc.scala: Has the classes for a regular expression accelerator
  - bitcount_accel.scala: A basic RoCC class to test against the popcount instruction
  - bitcount_scie.scala: The SCIE implementation of a popcount instruction
  - matrix.scala: Contains all the matrix multiplication designs
  - sqrt.scala: The SCIE implementation of the bit manipulation in fast inverse square root.
