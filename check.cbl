      ******************************************************************
      * Author: ZACHARY ALEXANDER DAVIS
      * Date: May 11th, 2025
      * Purpose: CHECK IF A WINNER EXISTS AND RETURN IT
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WINNER PIC X(1) VALUE SPACE.

       LINKAGE SECTION.
       01  TIC-TAC-TOE-BOARD.
           05 ROW OCCURS 3 TIMES.
               10 CELL OCCURS 3 TIMES.
                   15 CELL-VALUE PIC X VALUE SPACE.

       01  RETURN-WINNER PIC X(1).

       PROCEDURE DIVISION USING TIC-TAC-TOE-BOARD RETURN-WINNER.

      * Rows
           IF CELL-VALUE (1,1) = CELL-VALUE (1,2) AND
              CELL-VALUE (1,2) = CELL-VALUE (1,3) AND
              CELL-VALUE (1,1) NOT = SPACE
               MOVE CELL-VALUE (1,1) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

           IF CELL-VALUE (2,1) = CELL-VALUE (2,2) AND
              CELL-VALUE (2,2) = CELL-VALUE (2,3) AND
              CELL-VALUE (2,1) NOT = SPACE
               MOVE CELL-VALUE (2,1) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

           IF CELL-VALUE (3,1) = CELL-VALUE (3,2) AND
              CELL-VALUE (3,2) = CELL-VALUE (3,3) AND
              CELL-VALUE (3,1) NOT = SPACE
               MOVE CELL-VALUE (3,1) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

      * Columns
           IF CELL-VALUE (1,1) = CELL-VALUE (2,1) AND
              CELL-VALUE (2,1) = CELL-VALUE (3,1) AND
              CELL-VALUE (1,1) NOT = SPACE
               MOVE CELL-VALUE (1,1) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

           IF CELL-VALUE (1,2) = CELL-VALUE (2,2) AND
              CELL-VALUE (2,2) = CELL-VALUE (3,2) AND
              CELL-VALUE (1,2) NOT = SPACE
               MOVE CELL-VALUE (1,2) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

           IF CELL-VALUE (1,3) = CELL-VALUE (2,3) AND
              CELL-VALUE (2,3) = CELL-VALUE (3,3) AND
              CELL-VALUE (1,3) NOT = SPACE
               MOVE CELL-VALUE (1,3) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

      * Diagonals
           IF CELL-VALUE (1,1) = CELL-VALUE (2,2) AND
              CELL-VALUE (2,2) = CELL-VALUE (3,3) AND
              CELL-VALUE (1,1) NOT = SPACE
               MOVE CELL-VALUE (1,1) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

           IF CELL-VALUE (1,3) = CELL-VALUE (2,2) AND
              CELL-VALUE (2,2) = CELL-VALUE (3,1) AND
              CELL-VALUE (1,3) NOT = SPACE
               MOVE CELL-VALUE (1,3) TO WINNER
               MOVE WINNER TO RETURN-WINNER
               GOBACK
           END-IF

      * No winner found return SPACE
           MOVE SPACE TO RETURN-WINNER
           GOBACK.

       END PROGRAM CHECK.
