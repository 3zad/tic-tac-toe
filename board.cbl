      ******************************************************************
      * Author: ZACHARY ALEXANDER DAVIS
      * Date: May 10th, 2025
      * Purpose: PRINT THE TIC TAC TOE BOARD
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOARD.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  ROW-IDX      PIC 9 VALUE 1.
       77  COL-IDX      PIC 9 VALUE 1.

       LINKAGE SECTION.
       01  TIC-TAC-TOE-BOARD.
           05 ROW OCCURS 3 TIMES.
               10 CELL OCCURS 3 TIMES.
                   15 CELL-VALUE PIC X VALUE SPACE.

       PROCEDURE DIVISION USING TIC-TAC-TOE-BOARD.

           DISPLAY CELL-VALUE (2, 3)
           PERFORM VARYING ROW-IDX FROM 1 BY 1 UNTIL ROW-IDX > 3
               PERFORM VARYING COL-IDX FROM 1 BY 1 UNTIL COL-IDX > 3

                   DISPLAY CELL-VALUE (ROW-IDX, COL-IDX) WITH NO
                   ADVANCING

                   IF COL-IDX < 3
                       DISPLAY " | " WITH NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY ""   *> move to next line after each row
               IF ROW-IDX < 3
                   DISPLAY "--+---+--"
               END-IF
           END-PERFORM

           DISPLAY " "

      *----VOID FUNCTION NO RETURN
           EXIT PROGRAM.
       END PROGRAM BOARD.
