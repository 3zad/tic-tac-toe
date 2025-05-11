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

       77  WINNER            PIC X(1).

       01  RAND-FLOAT-X     USAGE COMP-1.
       01  RAND-INT-X       PIC 9.
       01  RAND-FLOAT-Y     USAGE COMP-1.
       01  RAND-INT-Y       PIC 9.

       01  TIC-TAC-TOE-BOARD.
           05 ROW OCCURS 3 TIMES.
               10 CELL OCCURS 3 TIMES.
                   15 CELL-VALUE PIC X VALUE SPACE.

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

           PERFORM FOREVER

               PERFORM FOREVER
      *------------Inefficient algorithm but whatever
                   COMPUTE RAND-FLOAT-X = FUNCTION RANDOM
                   COMPUTE RAND-INT-X = 1 + FUNCTION INTEGER
                   (RAND-FLOAT-X * 3)
                   COMPUTE RAND-FLOAT-Y = FUNCTION RANDOM
                   COMPUTE RAND-INT-Y = 1 + FUNCTION INTEGER
                   (RAND-FLOAT-Y * 3)

                   IF CELL-VALUE (RAND-INT-X, RAND-INT-Y) = SPACE
                       MOVE "O" TO
                       CELL-VALUE (RAND-INT-X, RAND-INT-Y)
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               CALL "BOARD" USING TIC-TAC-TOE-BOARD

               DISPLAY "YOUR MOVE (X,X): "
               ACCEPT PLAYER-MOVE

      *--------Clear screen
               DISPLAY X'1B' & "[2J" & X'1B' & "[H"

               MOVE PLAYER-MOVE(1:1) TO FIRST-NUMBER
               MOVE PLAYER-MOVE(2:2) TO SECOND-NUMBER

               MOVE "X" TO CELL-VALUE (FIRST-NUMBER, SECOND-NUMBER)

               CALL "CHECK" USING TIC-TAC-TOE-BOARD WINNER

               IF WINNER = "X" OR WINNER = "O"
                   DISPLAY WINNER " WON!"
                   EXIT PERFORM
               END-IF



           END-PERFORM

           STOP RUN.
       END PROGRAM TIC-TAC-TOE.
