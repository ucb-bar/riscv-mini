# Fetch tools for priv 1.7
git clone -n https://github.com/riscv/riscv-tools.git riscv-tools-priv1.7
cd riscv-tools-priv1.7
git checkout 4635ab67966c763a84f7217bc2c20b65dcabc7ec
git submodule update --init riscv-fesvr
git submodule update --init --recursive riscv-gnu-toolchain
git submodule update --init --recursive riscv-tests

# Build priv 1.7 RISC-V tools
source build.common

echo "Starting RISC-V Toolchain build process"

build_project riscv-fesvr --prefix=$RISCV
build_project riscv-gnu-toolchain --prefix=$RISCV --with-xlen=32
$MAKE -C riscv-tests/isa        RISCV_PREFIX=riscv32-unknown-elf- XLEN=32
$MAKE -C riscv-tests/benchmarks RISCV_PREFIX=riscv32-unknown-elf- XLEN=32
