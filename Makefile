base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main/scala/
gen_dir    = $(base_dir)/generated-src
log_dir    = $(base_dir)/logs
isa_dir    = $(base_dir)/riscv-tests/isa
bmarks_dir = $(base_dir)/riscv-bmarks

SBT       = sbt
SBT_FLAGS = -Dsbt.log.noformat=true -DchiselVersion=latest.release

include Makefrag-tests

Core-compile-cpp: $(gen_dir)/Core

Tile-compile-cpp: $(gen_dir)/Tile

$(gen_dir)/Core: $(wildcard $(src_dir)/*.scala)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run compile Core $(gen_dir) c"

$(gen_dir)/Tile: $(wildcard $(src_dir)/*.scala)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run compile Tile $(gen_dir) c"

Core-compile-v: $(wildcard $(src_dir)/*.scala)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run compile Core $(gen_dir) v"

Tile-compile-v: $(wildcard $(src_dir)/*.scala)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run compile Tile $(gen_dir) v"

$(addprefix Core-, $(isa_tests)): Core-%: $(isa_dir)/%.hex $(gen_dir)/Core
	mkdir -p $(log_dir)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run test Core $(gen_dir) $* +loadmem=$< +verbose +max-cycles=15000" \
	| tee $(log_dir)/$@.out

$(addprefix Tile-, $(isa_tests)): Tile-%: $(isa_dir)/%.hex $(gen_dir)/Tile
	mkdir -p $(log_dir)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run test Tile $(gen_dir) $* +loadmem=$< +verbose +max-cycles=15000" \
	| tee $(log_dir)/$@.out

$(addprefix Core-, $(bmarks)): Core-%: $(bmarks_dir)/%.hex $(gen_dir)/Core
	mkdir -p $(log_dir)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run test Core $(gen_dir) $* +loadmem=$< +max-cycles=1500000" \
	| tee $(log_dir)/$@.out

$(addprefix Tile-, $(bmarks)): Tile-%: $(bmarks_dir)/%.hex $(gen_dir)/Tile
	mkdir -p $(log_dir)
	cd $(base_dir) ; $(SBT) $(SBT_FLAGS) "run test Tile $(gen_dir) $* +loadmem=$< +max-cycles=1500000" \
	| tee $(log_dir)/$@.out

clean:
	rm -rf $(gen_dir) $(log_dir) test-outputs *.key

cleanall: clean
	rm -rf target project/target

.PHONY: clean cleanall
