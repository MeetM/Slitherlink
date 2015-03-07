#Slitherlink - game and solver

The game is written in clisp. Has been tested on Ubuntu and Windows Clisp.

To play the game  

1. Go into clisp prompt. For ubuntu, go to terminal and type `clisp`

2. Type the following commands  
`(compile-file "path/to/slither.lisp")`  
`(load "path/to/slither.fas")`  
`(slither)`  

3. An introduction to the game will be shown with instructions on how to play the game. At the prompt, you will be asked to give path for the input board file.  
      `Enter input filepath : path/to/file`  
   The tournament boards are in the "boards" directory. The name of the files are the just the number. (Eg. 1 , 2 , 3 ,.... , 15)  

4. After giving the input file path, you will be shown the intial board of the game and you will be asked whether you want to play it manually or want the program to solve it for you.  
   If you choose to play it yourself, then jump to step 6.  
   Else if you select automatic, continue from next step.  

5. It will take some time to solve the board depending on the size. While it is solving, it will show a PLEASE WAIT message. When solved, it will show the solved board and the time it took to solve it. Jump to step 9.  

6. At the prompt, you will be asked to specify your move.  
###Moves  
   To draw a line around a cell, its position in the board (x,y co-ordinates)
   followed by the direction where you want to draw it (T -> Top, D ->  Down, R -> Right, L -> Left)   
   Example: For line on top of cell 2,2; type -> 2 2 T  
   For removing a line, type the same move again.  
   To Quit, type -> Q  

7. After each move, the program will display the current board structure and status whether or not you have solved the puzzle.  

8. After you have solved the puzzle, the program will exit.  

9. If you want to play again. Repeat from step 3.

####Note for the input file format

The input board file (if you wish to try your own boards,) should be specified in the following format

    + + + +
     3 2 3
    + + + +
          
    + + + +

When there is no number in a cell, you should indicate it by a blank space.
