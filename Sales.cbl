       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO DISK
               FILE STATUS IS SALES-FILE-STATUS.

           SELECT RESULTS-FILE ASSIGN TO DISK
               FILE STATUS IS RESULTS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 12 CHARACTERS
           DATA RECORD IS SALES-REC.
       01  SALES-REC.
           05 SALES-AMOUNT        PIC 9(10).
           05 SALES-CRLF          PIC X(02).



       WORKING-STORAGE SECTION.
       01  SALES-FILE-STATUS             PIC 9(02) VALUE ZERO.
       01  RESULTS-FILE-STATUS           PIC 9(02) VALUE ZERO.
       01  DATA-REMAINS-SWITCH             PIC X(02) VALUE SPACES.
           88 NO-MORE-DATA                 VALUE 'NO'.
       01  REGION-NAMES.
           05  REGION-NAME OCCURS 9 TIMES PIC X(10) VALUE SPACES.
       01  TWOTABS-TABLE.
           05 REGIONS  OCCURS 9 TIMES.
              10 MONTHLY-SALES OCCURS 12 TIMES PIC 9(05).
       01  ROW-TOTAL      PIC 9(8).
       01  COL-TOTAL      PIC 9(8).
       01  DISPLAY-SALE   PIC ZZZZ9.
       01  DISPLAY-TOTAL  PIC ZZZZZZ9.


       01  IDI                  PIC 9(3).
       01  IDJ                  PIC 9(03).
       01  IDX                  PIC 9(03) VALUE 0.

       PROCEDURE DIVISION.
           PERFORM OPEN-FILES
           PERFORM PROCESS-SALES
           PERFORM DISPLAY-SALES-TABLE
           PERFORM WRITE-SALES-TABLE
           PERFORM FINISH.

       OPEN-FILES.
           OPEN INPUT SALES-FILE
               OUTPUT RESULTS-FILE
           IF SALES-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' SALES-FILE-STATUS
               PERFORM FINISH.
           IF RESULTS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: VALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' RESULTS-FILE-STATUS
               PERFORM FINISH.

       PROCESS-SALES.
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI > 9
              PERFORM VARYING IDJ FROM 1 BY 1 UNTIL IDJ > 12
                 READ SALES-FILE
                  AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
                 END-READ
                 MOVE SALES-AMOUNT TO MONTHLY-SALES(IDI, IDJ)
              END-PERFORM
           END-PERFORM.
       DISPLAY-SALES-TABLE.
           MOVE 'YPIROS'     TO REGION-NAME(1)
           MOVE 'THESSALIA'  TO REGION-NAME(2)
           MOVE 'THRAKI'  TO REGION-NAME(3)
           MOVE 'CRETE'     TO REGION-NAME(4)
           MOVE 'MACEDONIA' TO REGION-NAME(5)
           MOVE 'AIGAIO'      TO REGION-NAME(6)
           MOVE 'IONIO'     TO REGION-NAME(7)
           MOVE 'PELOPONISOS'    TO REGION-NAME(8)
           MOVE 'STEREA'     TO REGION-NAME(9)
           DISPLAY 'REGION        JAN   FEB   MAR   APR   MAY   JUN   '
      -            'JUL   AUG   SEP   OCT   NOV   DEC    TOTAL'
           DISPLAY '--------------------------------------------------'
      -     '-------------------------------------------'
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI > 9

               MOVE 0 TO ROW-TOTAL

               DISPLAY REGION-NAME(IDI) '  ' NO ADVANCING

               PERFORM VARYING IDJ FROM 1 BY 1 UNTIL IDJ > 12
                MOVE MONTHLY-SALES(IDI,IDJ) TO DISPLAY-SALE
                DISPLAY DISPLAY-SALE SPACE NO ADVANCING
                ADD MONTHLY-SALES(IDI,IDJ) TO ROW-TOTAL
               END-PERFORM
               DISPLAY '|'  NO ADVANCING
               MOVE ROW-TOTAL TO DISPLAY-TOTAL
               DISPLAY DISPLAY-TOTAL
           END-PERFORM
           DISPLAY '--------------------------------------------------'
      -     '-------------------------------------------'
           DISPLAY 'TOTAL       ' NO ADVANCING
           PERFORM VARYING IDJ FROM 1 BY 1 UNTIL IDJ > 12
               MOVE 0 TO COL-TOTAL

               PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI > 9
                ADD MONTHLY-SALES(IDI,IDJ) TO COL-TOTAL
               END-PERFORM

               MOVE COL-TOTAL TO DISPLAY-SALE
               DISPLAY DISPLAY-SALE SPACE NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       WRITE-SALES-TABLE.

       FINISH.
           CLOSE SALES-FILE
                 RESULTS-FILE
           STOP RUN.
       END PROGRAM SALES.
