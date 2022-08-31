// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

// Put your code here.
    // init i=0
    @i
    M=0
    // init total=0
    @R2
    M=0
(LOOP)
    // load i
    @i
    D=M
    // goto END if multiplier - i <= 0
    @R1
    D=M-D
    @END
    D;JLE
    // reset D to R0
    @R0
    D=M
    // add R0 to total 
    @R2
    M=D+M
    // increment i
    @i
    M=M+1
    // goto LOOP
    @LOOP
    0;JMP
(END)
    @END
    0;JMP