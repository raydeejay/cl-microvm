# cl-microvm
### _Sergi Reyner <sergi.reyner@gmail.com>_

CL-MICROVM is a very simple VM and a set of tools to work with it. It
models an imaginary architecture, with the following characteristics.

## Characteristics

- 32 instructions with 0/1/2 operands, either 8-bit or 16-bit wide.
- 64Kb of memory
- Unlimited stack

## Tools

- Assembler, in the form of a macro
- Memory dumper

NOTE: Not all instructions are implemented yet, so far only PRN, CALL
and RET are. In the same way, the specifications are still in flux.


## License

MIT License
