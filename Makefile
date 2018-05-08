default: compile

base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main/scala/
gen_dir    = $(base_dir)/generated-src

SBT       = sbt
SBT_FLAGS = -ivy $(base_dir)/.ivy2

sbt:
	$(SBT) $(SBT_FLAGS)

compile: $(gen_dir)/Tile.v

$(gen_dir)/Tile.v: $(wildcard $(src_dir)/*.scala)
	$(SBT) $(SBT_FLAGS) "run $(gen_dir)"

test:
	$(SBT) $(SBT_FLAGS) test

clean:
	rm -rf $(gen_dir) test_run_dir

cleanall: clean
	rm -rf target project/target

.PHONY: clean cleanall
