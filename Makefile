default: compile

base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main/scala/
gen_dir    = $(base_dir)/generated-src
log_dir    = $(base_dir)/logs
isa_dir    = $(base_dir)/riscv-tests/isa
bmarks_dir = $(base_dir)/riscv-bmarks

SBT       = sbt
SBT_FLAGS = -DchiselVersion=latest.release

compile: $(gen_dir)/Tile.v

$(gen_dir)/Tile.v: $(wildcard $(src_dir)/*.scala)
	cd $(base_dir) && $(SBT) $(SBT_FLAGS) "run $(gen_dir)"

clean:
	rm -rf $(gen_dir) test-* *.key

cleanall: clean
	rm -rf target project/target

.PHONY: clean cleanall
