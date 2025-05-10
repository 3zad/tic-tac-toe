      ******************************************************************
      * Author: ZACHARY ALEXANDER DAVIS
      * Date: MAY 10TH, 2025
      * Purpose: TIC TAC TOE GAME
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TIC-TAC-TOE.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01  PLAYER-MOVE PIC X(4).

       77  FIRST-NUMBER      PIC 99.
       77  SECOND-NUMBER     PIC 99.

       01  TIC-TAC-TOE-BOARD.
           05 ROW OCCURS 3 TIMES.
               10 CELL OCCURS 3 TIMES.
                   15 CELL-VALUE PIC X VALUE SPACE.

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **

           PERFORM FOREVER
               DISPLAY "YOUR MOVE (X,X): "
               ACCEPT PLAYER-MOVE

               MOVE PLAYER-MOVE(1:1) TO FIRST-NUMBER
               MOVE PLAYER-MOVE(2:2) TO SECOND-NUMBER

               DISPLAY FIRST-NUMBER
               DISPLAY SECOND-NUMBER

               MOVE "X" TO CELL-VALUE (FIRST-NUMBER, SECOND-NUMBER)

               CALL "BOARD" USING TIC-TAC-TOE-BOARD

           END-PERFORM

            STOP RUN.
      ** add other procedures here
       END PROGRAM TIC-TAC-TOE.
