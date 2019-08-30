#!/bin/bash
iverilog -o TOPModule.vvp -Dverilator -DPRINTF_COND=1 -DRANDOMIZE_GARBAGE_ASSIGN -DRANDOMIZE_INVALID_ASSIGN -DRANDOMIZE_REG_INIT -DRANDOMIZE_MEM_INIT TOPModule.v TOPModule.ram.v Testbench.v
vvp TOPModule.vvp

