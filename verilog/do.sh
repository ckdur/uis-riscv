#!/bin/bash
cd ..
sbt "compile"
sbt "compile"
sbt "runMain generator.Main"
sbt "runMain generator.Main"
cd verilog
./vlsi_mem_gen ./TOPModule.ram.conf > ./TOPModule.ram.v

