default: compile

src_dir    = $(abspath .)/src/main
gen_dir    = $(abspath .)/generated-src
out_dir    = outputs

SBT       = sbt
SBT_FLAGS = -ivy ./.ivy2

# sbt
sbt:
	$(SBT) $(SBT_FLAGS)

# generate RTL from Chisel
$(gen_dir)/Tile.v: $(wildcard $(src_dir)/scala/*.scala)
	$(SBT) $(SBT_FLAGS) "run $(gen_dir)"

compile: $(gen_dir)/Tile.v

# build verilator simulation binary
CXXFLAGS += -std=c++11 -Wall -Wno-unused-variable
VERILATOR = verilator --cc --exe
VERILATOR_FLAGS = --assert -Wno-STMTDLY -O3 --trace \
	--top-module Tile -Mdir $(gen_dir)/VTile.csrc \
	-CFLAGS "$(CXXFLAGS) -include $(gen_dir)/VTile.csrc/VTile.h"

VTile: $(gen_dir)/Tile.v $(src_dir)/cc/top.cc $(src_dir)/cc/mm.cc $(src_dir)/cc/mm.h
	$(VERILATOR) $(VERILATOR_FLAGS) -o $@ $< $(word 2, $^) $(word 3, $^)
	$(MAKE) -C $(gen_dir)/VTile.csrc -f VTile.mk
	ln -s generated-src/VTile.csrc/VTile VTile

verilator: ./VTile

# run ISA tests in riscv-tests
rv32ui = $(wildcard riscv-tests/isa/rv32ui/*.S)
rv32mi = $(wildcard riscv-tests/isa/rv32mi/*.S)
rv32ui_bin = $(patsubst riscv-tests/isa/rv32ui/%.S,riscv-tests/isa/rv32ui-p-%,$(rv32ui))
rv32mi_bin = $(patsubst riscv-tests/isa/rv32mi/%.S,riscv-tests/isa/rv32mi-p-%,$(rv32mi))
rv32ui_hex = $(addsuffix .hex,$(rv32ui_bin))
rv32mi_hex = $(addsuffix .hex,$(rv32mi_bin))
isa_tests_hex = $(rv32ui_hex) $(rv32mi_hex)
isa_tests_out = $(patsubst riscv-tests/isa/%.hex,outputs/isa/%.out,$(isa_tests_hex))

$(rv32ui_bin):
	$(MAKE) -C riscv-tests/isa rv32ui
$(rv32mi_bin):
	$(MAKE) -C riscv-tests/isa rv32mi

riscv-tests/isa/%.hex: riscv-tests/isa/% $(rv32ui_bin) $(rv32mi_bin)
	riscv64-unknown-elf-elf2hex --bit-width 32 --input $< --output $@

$(out_dir)/isa/%.out: ./VTile riscv-tests/isa/%.hex
	mkdir -p $(out_dir)/isa
	-./$^ $(patsubst %.out,%.vcd,$@) 2>&1 | tee -a $@

isa-tests-hex: $(isa_tests_hex)
isa-tests: $(isa_tests_out)

# isa tests + benchmarks with verilator
# test_hex_files = $(wildcard $(base_dir)/tests/*.hex)
# test_out_files = $(foreach f,$(test_hex_files),$(patsubst %.hex,%.out,$(out_dir)/$(notdir $f)))
# 
# $(test_out_files): $(out_dir)/%.out: $(base_dir)/VTile $(base_dir)/tests/%.hex
# 	mkdir -p $(out_dir)
# 	$^ $(patsubst %.out,%.vcd,$@) 2> $@
# 
# run-tests: $(test_out_files)
# 
# # run custom benchamrk
# custom_bmark_hex ?= $(base_dir)/custom-bmark/main.hex
# custom_bmark_out  = $(patsubst %.hex,%.out,$(out_dir)/$(notdir $(custom_bmark_hex)))
# $(custom_bmark_hex):
# 	$(MAKE) -C custom-bmark
# 
# $(custom_bmark_out): $(base_dir)/VTile $(custom_bmark_hex)
# 	mkdir -p $(out_dir)
# 	$^ $(patsubst %.out,%.vcd,$@) 2> $@
# 
# run-custom-bmark: $(custom_bmark_out)

# unit tests + integration tests
test:
	$(SBT) $(SBT_FLAGS) test

clean:
	rm -rf $(gen_dir) $(out_dir) test_run_dir

cleanall: clean
	rm -rf target project/target

.PHONY: sbt compile verilator isa-tests isa-tests-hex run-custom-bmark test clean cleanall
