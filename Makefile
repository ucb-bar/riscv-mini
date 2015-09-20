base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main/scala/designs
gen_dir    = $(base_dir)/generated-src
log_dir    = $(base_dir)/logs
isa_dir    = $(base_dir)/riscv-tests/isa
bmarks_dir = $(base_dir)/riscv-bmarks

SBT       = sbt
SBT_FLAGS = -Dsbt.log.noformat=true -DchiselVersion=latest.release

C_FLAGS     := --targetDir $(gen_dir) --genHarness --compile --test --minimumCompatibility 3.0
V_FLAGS     := $(C_FLAGS) --v
DEBUG_FLAGS := --vcd --vcdMem --debug

VPATH = $(src_dir):$(gen_dir):$(log_dir)

include Makefrag-tests
include Makefrag

clean:
	rm -rf $(gen_dir) $(log_dir) *.key

cleanall: clean
	rm -rf target project/target

.PHONY: clean cleanall

