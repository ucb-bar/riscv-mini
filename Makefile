default: compile

base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main/scala/
gen_dir    = $(base_dir)/generated-src

SBT       = sbt
SBT_FLAGS = -Dchisel3Version=3.1-SNAPSHOT -DfirrtlVersion=1.1-SNAPSHOT \
	-Dchisel-iotesterVersion=1.2-SNAPSHOT

sbt:
	$(SBT) $(SBT_FLAGS)

compile: $(gen_dir)/Tile.v

$(gen_dir)/Tile.v: $(wildcard $(src_dir)/*.scala)
	$(SBT) $(SBT_FLAGS) "run $(gen_dir)"

test:
	$(SBT) $(SBT_FLAGS) test

clean:
	rm -rf $(gen_dir) test-* *.key

cleanall: clean
	rm -rf target project/target

.PHONY: clean cleanall
