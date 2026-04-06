       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO DISK
               FILE STATUS IS SALES-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 12 CHARACTERS
           DATA RECORD IS SALES-REC.
       01  SALES-REC.
           05 SALES-AMOUNT               PIC 9(10).
           05 SALES-CRLF                 PIC X(02).
       WORKING-STORAGE SECTION.
       01  SALES-FILE-STATUS             PIC 9(02) VALUE ZERO.
       01  DATA-REMAINS-SWITCH           PIC X(02) VALUE SPACES.
           88 NO-MORE-DATA               VALUE 'NO'.
       01  REGION-NAMES.
           05  REGIONS.
              10 FILLER PIC X(11)     VALUE 'YPIROS'.
              10 FILLER PIC X(11)     VALUE 'THESSALIA'.
              10 FILLER PIC X(11)     VALUE 'THRAKI'.
              10 FILLER PIC X(11)     VALUE 'CRETE'.
              10 FILLER PIC X(11)     VALUE 'MACEDONIA'.
              10 FILLER PIC X(11)     VALUE 'AIGAIO'.
              10 FILLER PIC X(11)     VALUE 'IONIO'.
              10 FILLER PIC X(11)     VALUE 'PELOPONISOS'.
              10 FILLER PIC X(11)     VALUE 'STEREA'.
              10 FILLER PIC X(11)     VALUE 'TOTALS'.
           05  REGION-NAME REDEFINES REGIONS OCCURS 10
               TIMES PIC X(11).
       01  MONTH-NAMES.
           05 MONTH.
              10 FILLER    PIC X(07) VALUE 'JAN.|'.
              10 FILLER    PIC X(07) VALUE 'FEB.|'.
              10 FILLER    PIC X(07) VALUE 'MAR.|'.
              10 FILLER    PIC X(07) VALUE 'APR.|'.
              10 FILLER    PIC X(07) VALUE 'MAY |'.
              10 FILLER    PIC X(07) VALUE 'JUN.|'.
              10 FILLER    PIC X(07) VALUE 'JUL.|'.
              10 FILLER    PIC X(07) VALUE 'AUG.|'.
              10 FILLER    PIC X(07) VALUE 'SEP.|'.
              10 FILLER    PIC X(07) VALUE 'OCT.|'.
              10 FILLER    PIC X(07) VALUE 'NOV.|'.
              10 FILLER    PIC X(07) VALUE 'DEC.|'.
           05 MONTH-NAME REDEFINES MONTH OCCURS 12 TIMES PIC X(07).
       01  TWOTABS-TABLE.
           05 REGIONS  OCCURS 10 TIMES.
              10 MONTHLY-SALES OCCURS 13 TIMES PIC 9(06).

       01  DISPLAY-SALE                  PIC ZZZZZ9.
       01  DISPLAY-TOTAL                 PIC ZZZZZZ9.
       01  ROW-TOTAL                     PIC 9(8)  VALUE 0.
       01  COL-TOTAL                     PIC 9(8)  VALUE 0.
       01  GRAND-TOTAL                   PIC 9(8) VALUE 0.
       01  PRINT-GRAND-TOTAL             PIC ZZZ.ZZ9.
       01  IDI                           PIC 9(3).
       01  IDJ                           PIC 9(03).
       01  IDX                           PIC 9(03) VALUE 0.

       PROCEDURE DIVISION.
           PERFORM OPEN-FILES
           INITIALIZE TWOTABS-TABLE
           READ SALES-FILE
           AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ
           PERFORM PROCESS-SALES
           IF NO-MORE-DATA AND IDI <> 10 OR IDJ <> 13
              DISPLAY '**ERROR! INPUT FILE CONTAINS LESS DATA THAN'
                      ' INTERNAL TABLE CAN PROCESS!!! CORRECT ITABLE...'
              MOVE 21 TO RETURN-CODE
              PERFORM FINISH
           END-IF
           IF  NOT NO-MORE-DATA
               DISPLAY '***ERROR!!! INPUT FILE CONTAINS MORE DATA THAN'
                      ' INTERNAL TABLE CAN PROCESS!!! CORRECT ITABLE...'
              MOVE 20 TO RETURN-CODE
              PERFORM FINISH
           END-IF.
           PERFORM DISPLAY-SALES-TABLE
           PERFORM FINISH.

       OPEN-FILES.
           OPEN INPUT SALES-FILE
           IF SALES-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: SALES-FILE'
               DISPLAY 'STATUS-CODE=' SALES-FILE-STATUS
               PERFORM FINISH.

       PROCESS-SALES.
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI > 9 OR
               NO-MORE-DATA
              PERFORM VARYING IDJ FROM 1 BY 1 UNTIL IDJ > 12
                  ADD SALES-AMOUNT TO MONTHLY-SALES(IDI, IDJ)
                                      MONTHLY-SALES(IDI, 13 )
                                      MONTHLY-SALES(10,  IDJ)
                                      MONTHLY-SALES(10,  13)
                  READ SALES-FILE
                       AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
                  END-READ
              END-PERFORM
          END-PERFORM.

       DISPLAY-SALES-TABLE.
           DISPLAY '----------------------------------------------------
      -             '---------------------------------------------------
      -            '----'
           DISPLAY 'REGION      |  ' NO ADVANCING
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 12
             DISPLAY MONTH-NAME(IDX) NO ADVANCING
           END-PERFORM.
           DISPLAY 'TOTALS |'
           DISPLAY '----------------------------------------------------
      -             '---------------------------------------------------
      -            '----'
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI > 10
               MOVE 0 TO ROW-TOTAL
               IF IDI = 10
                  DISPLAY '---------------------------------------------
      -                   '---------------------------------------------
      -                   '-----------------'
               END-IF
               DISPLAY REGION-NAME(IDI) ' |' NO ADVANCING
               PERFORM VARYING IDJ FROM 1 BY 1 UNTIL IDJ > 13
                   MOVE MONTHLY-SALES(IDI,IDJ) TO DISPLAY-SALE
                   DISPLAY DISPLAY-SALE '|' NO ADVANCING
                   IF IDJ = 12
                       DISPLAY '   ' NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY ' '
           END-PERFORM.
           DISPLAY '--------------------------------------------------'
      -     '---------------------------------------------------------'.
       FINISH.
           CLOSE SALES-FILE
           STOP RUN.
       END PROGRAM SALES.
