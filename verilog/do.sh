#!/bin/bash
# Generate Verilog from scala code
cd ..
sbt "compile"
sbt "compile"
sbt "runMain generator.Main"
sbt "runMain generator.Main"
cd verilog
# The current flow generates instanced rams instead of integrated 
# rams. We generate the verilog for the instanced ram here
./vlsi_mem_gen ./TOPModule.ram.conf > ./TOPModule.ram.v
# Compile the bootloader (just a dummy bootloader)
riscv64-unknown-elf-gcc -I./bootloader -c ./bootloader/start.S -o ./bootloader/start.o -march=rv32i -mabi=ilp32
riscv64-unknown-elf-gcc -I./bootloader -c ./bootloader/main.c -o ./bootloader/main.o -march=rv32i -mabi=ilp32
riscv64-unknown-elf-gcc ./bootloader/start.o ./bootloader/main.o -o ./bootloader/boot.elf -march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -Wl,-Bstatic,-T,./bootloader/ram.lds,-Map,./bootloader/boot.map,--strip-debug
# Just dump the assembly, for fun
riscv64-unknown-elf-objdump -D ./bootloader/boot.elf -M no-aliases,numeric > ./bootloader/boot.noaliases.dump
riscv64-unknown-elf-objdump -D ./bootloader/boot.elf > ./bootloader/boot.dump
# Get the binary machine code
riscv64-unknown-elf-objcopy -O binary ./bootloader/boot.elf ./bootloader/boot.bin
# Get the silly hex file for verilog
od -t x4 -An -w4 -v ./bootloader/boot.bin > boot.hex

