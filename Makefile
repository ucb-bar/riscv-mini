base_dir = $(abspath .)

include Makefrag

clean:
	rm -rf $(gen_dir) $(log_dir) *.key

cleanall: clean
	rm -rf target project/target

.PHONY: clean cleanall

