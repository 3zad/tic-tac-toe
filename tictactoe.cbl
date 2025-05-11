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
       01  IS-ILLEGAL-MOVE PIC X(1).

       77  FIRST-NUMBER      PIC 99.
       77  SECOND-NUMBER     PIC 99.

       77  WINNER            PIC X(1).

       77  SEED-VALUE   PIC 9(9).
       77  RAND-FLOAT-X     USAGE COMP-1.
       77  RAND-INT-X       PIC 9.
       77  RAND-FLOAT-Y     USAGE COMP-1.
       77  RAND-INT-Y       PIC 9.

       77  CURR-TIME    PIC 9(9).

       01  TIC-TAC-TOE-BOARD.
           05 ROW OCCURS 3 TIMES.
               10 CELL OCCURS 3 TIMES.
                   15 CELL-VALUE PIC X VALUE SPACE.

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

           ACCEPT CURR-TIME FROM TIME
           MOVE FUNCTION NUMVAL(CURR-TIME) TO SEED-VALUE

      *----Seed the generator
           COMPUTE RAND-FLOAT-X = FUNCTION RANDOM(SEED-VALUE)

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


               MOVE "0" TO IS-ILLEGAL-MOVE

               PERFORM FOREVER
                   PERFORM CLEAR-SCREEN-PROCEDURE

                   PERFORM CHECK-WINNER-PROCEDURE

                   CALL "BOARD" USING TIC-TAC-TOE-BOARD

                   IF IS-ILLEGAL-MOVE = "1"
                       DISPLAY "ILLEGAL MOVE! TRY AGAIN!"
                   END-IF

                   DISPLAY "YOUR MOVE (X,X): "
                   ACCEPT PLAYER-MOVE

                   MOVE "0" TO IS-ILLEGAL-MOVE

                   MOVE PLAYER-MOVE(1:1) TO FIRST-NUMBER
                   MOVE PLAYER-MOVE(2:2) TO SECOND-NUMBER

                   IF CELL-VALUE (FIRST-NUMBER, SECOND-NUMBER) = SPACE
                       EXIT PERFORM
                   END-IF

                   MOVE "1" TO IS-ILLEGAL-MOVE

               END-PERFORM

               MOVE "X" TO CELL-VALUE (FIRST-NUMBER, SECOND-NUMBER)

               PERFORM CHECK-WINNER-PROCEDURE

           END-PERFORM

           STOP RUN.

       CLEAR-SCREEN-PROCEDURE.
           DISPLAY X'1B' & "[2J" & X'1B' & "[H".

       CHECK-WINNER-PROCEDURE.
           CALL "CHECK" USING TIC-TAC-TOE-BOARD WINNER

           IF WINNER = "X" OR WINNER = "O"
               PERFORM CLEAR-SCREEN-PROCEDURE

               CALL "BOARD" USING TIC-TAC-TOE-BOARD

               DISPLAY WINNER " WON!"
               STOP RUN
           END-IF.

       END PROGRAM TIC-TAC-TOE.
