// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.
    @SCREEN
    D=A

    //R0: pixel counter
    @R0
    M=D

    //R1: max pixel
    @8192
    D=D+A
    @R1
    M=D

(LOOP)
    //R2: pixel value
    @R2
    M=0

    @KBD
    D=M
    @DRAW
    D;JEQ
    @R2
    M=-1

(DRAW)
    // load pixel value to D
    @R2
    D=M
    // load pixel counter to A
    @R0
    A=M
    // move D (pixel value) to M (pixel counter address)
    M=D
    // inc pixel counter
    @R0
    M=M+1
    
    @R1
    D=M
    @R0
    D=D-M
    @LOOP
    D;JGT

    @SCREEN
    D=A
    @R0
    M=D

    @LOOP
    0;JMP
   
    