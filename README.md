# riscv-mini

Author: Donggyu Kim (dgkim@eecs.berkeley.edu)

`riscv-mini` is a simple RISC-V 3-stage pipeline written in Chisel. It has been a crucial example in various project developments,
including [Chisel3](https://github.com/ucb-bar/chisel3.git), [FIRRTL](https://github.com/ucb-bar/firrtl.git),
[Strober](https://bar.eecs.berkeley.edu/projects/strober.html), simulation and verification methodologies.
It implements RV32I of the User-level ISA Version 2.0 and the Machine-level ISA of the Privileged Architecture Version 1.7.
Unlike other simple pipelines, it also contains simple instruction and data caches.

Note that a real-world processor is not the goal of `riscv-mini`.
It is developed as an intermediate example before diving into [rocket-chip](https://github.com/freechipsproject/rocket-chip). 

## Datapath Diagram
![pipeline](diagram.png)

## Getting Started

```shell
$ git clone https://github.com/ucb-bar/riscv-mini.git
$ cd riscv-mini
$ make            # generate firrtl & verilog files in generated-src
```

The verilog output file can be used for verilator simulation or the ASIC tool flow.

## Running Verilator Simulation

First, generate the verilator binary:

```shell
$ make verilator
```

This will generate `VTile` in the top-level directory.

Now, you can run verilator simulation for a given hex file as follows:

```shell
$ ./VTile <hex file> [<vcd file> 2> <log file>]
```

`<vcd file>` and the pipe to `<log file>` are optional. The waveform is dumped to `dump.vcd` and the execution trace is printed in the screen by default.

### riscv-tests

#### Prerequisites
1. Download the [latest RISC-V GCC toolchain from SiFive](https://www.sifive.com/software) (see 'Prebuilt RISC-V GCC Toolchain and Emulator) and install them by unpacking the archive.
2. Clone and build [`elf2hex`](https://github.com/sifive/elf2hex) according to the instructions in its README.

To run the ISA tests (rv32ui, rv32mi) and benchmarks from [`riscv-tests`](https://github.com/riscv-software-src/riscv-tests), clone the submodule:

```shell
git submodule update --init --recursive .
```

Then compile the tests (which will appear in `riscv-tests/isa/rv32{u,m}i-p-<test_name>.{dump,hex}`):

```shell
make isa-tests-hex
```

and run the tests using the Verilator simulator:

```shell
make isa-tests
```

The test outputs (stdout/stderr log and VCD waveform) will appear in `outputs/isa/rv32{u,m}i-p-<test_name>.{out,vcd}`.
You can check the test results using grep:

```shell
grep -r "FAIL" outputs/isa
grep -r "PASS" outputs/isa
```

## Unit and Integration Tests with `sbt`

`riscv-mini` provides synthesizable unit & integration tests.
Theres are six sets of unit tests(`ALUTests`, `BrCondTests`, `ImmGenTests`, `CSRTests`, `CacheTests`, `DatapathTests`),
running user-defined test vectors.
To execute them, first launch sbt with `make sbt` and run:

    > testOnly mini.[testname]
  
There are also six sets of integration tests, running the hex files from [riscv-tests](https://github.com/riscv/riscv-tests).
To execute them, also launch `sbt` and run:

    > testOnly mini.[Core|Tile][Simple|ISA|Bmark]Tests
    
`Core` only contains the datapath and the control unit, while `Tile` also contains I$ and D$. `Simple` only runs `rv32ui-p-simple`,
`ISA` runs the whole ISA tests, and `Bmark` runs five benchmarks(`median`, `multiply`, `qsort`, `towers`, `vvadd`). 
Note that all tests in a set run in parallel.

Finally, to run all the tests, just in sbt:

    > test
    
## Running Your Own Program on `riscv-mini`

At this point, you may want to implement and exeucte your custom application on `riscv-mini`. In this case, you need to install RISC-V tools for priv 1.7. This repo provides a script to install the correct version of tools. Run the script as follows:

    $ export RISCV=<path to riscv tools for priv 1.7>
    $ ./build-riscv-tools
    
It takes a while to install the toolchain, so please be patient.

This repo also provides a template for your own program in `custom-bmark`. Add your c or assembly code and edit `Makefile`. Next, to compile you program, run `make` in `custom-bmark` to generate the binary, dump, and the hex files. Finally, run the following command in the base directory:

    $ make run-custom-bmark
