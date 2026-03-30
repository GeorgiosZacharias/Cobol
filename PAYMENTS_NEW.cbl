       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLLS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO DISK
               FILE STATUS IS PAYROLL-FILE-STATUS.
           SELECT VALID-PAYROLL-FILE ASSIGN TO DISK
               FILE STATUS IS VALID-PAYROLL-FILE-STATUS.
           SELECT INVALID-PAYROLL-FILE ASSIGN TO DISK
               FILE STATUS IS INVALID-PAYROLL-FILE-STATUS.
           SELECT STATS-FILE ASSIGN TO DISK
               FILE STATUS IS STATS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 38 CHARACTERS
           DATA RECORD IS PAYROLL-IN.
       01  PAYROLL-IN.
           05 PAYROLL-EMPLOYEE-NAME        PIC X(20).
           05 PAYROLL-HOURS-WORKED         PIC 9(03).
           05 PAYROLL-HOUR-RATE            PIC 99V99.
           05 PAYROLL-AFM                  PIC X(09).
           05 PAYROLL-CRLF                 PIC X(02).

       FD  VALID-PAYROLL-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 177 CHARACTERS
           DATA RECORD IS VALID-LINE.
       01  VALID-LINE.
           05 VALID-EMPLOYEE-NAME          PIC X(20).
           05 VALID-FILLER-1               PIC X(01).
           05 VALID-HOURS-WORKED           PIC 9(03).
           05 VALID-FILLER-2               PIC X(10).
           05 VALID-HOUR-RATE              PIC 99,99.
           05 VALID-FILLER-3               PIC X(10).
           05 VALID-AFM                    PIC X(09).
           05 VALID-FILLER-4               PIC X(10).
           05 VALID-ANNUAL-AMOUNT          PIC Z.ZZZ.ZZ9,99.
           05 VALID-FILLER-5               PIC X(10).
           05 VALID-TAX                    PIC Z.ZZ9,99.
           05 VALID-FILLER-6               PIC X(10).
           05 VALID-NET-AMOUNT             PIC Z.ZZZ.ZZ9,99.
           05 VALID-FILLER-7               PIC X(55).
           05 VALID-CRLF                   PIC X(02).

       FD  INVALID-PAYROLL-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 177 CHARACTERS
           DATA RECORD IS INVALID-LINE.
       01  INVALID-LINE.
           05 INVALID-EMPLOYEE-NAME        PIC X(20).
           05 INVALID-FILLER-1             PIC X(01).
           05 INVALID-HOURS-WORKED         PIC 9(03).
           05 INVALID-FILLER-2             PIC X(10).
           05 INVALID-HOUR-RATE            PIC 99,99.
           05 INVALID-FILLER-3             PIC X(10).
           05 INVALID-AFM                  PIC X(09).
           05 INVALID-FILLER-4             PIC X(10).
           05 INVALID-ANNUAL-AMOUNT        PIC Z.ZZZ.ZZ9,99.
           05 INVALID-FILLER-5             PIC X(10).
           05 INVALID-TAX                  PIC Z.ZZ9,99.
           05 INVALID-FILLER-6             PIC X(10).
           05 INVALID-NET-AMOUNT           PIC Z.ZZZ.ZZ9,99.
           05 INVALID-FILLER-7             PIC X(11).
           05 INVALID-REASON               PIC X(44).
           05 INVALID-CRLF                 PIC X(02).

       FD  STATS-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 44 CHARACTERS
           DATA RECORD IS STATS-LINE.
       01  STATS-LINE.
           05 STATS-RECORD                 PIC X(30).
           05 STATS-VALUE                  PIC Z.ZZZ.ZZ9,ZZ.
           05 STATS-CRLF                   PIC X(02).

       WORKING-STORAGE SECTION.
       01  PAYROLL-FILE-STATUS             PIC 9(02) VALUE ZERO.
       01  VALID-PAYROLL-FILE-STATUS       PIC 9(02) VALUE ZERO.
       01  INVALID-PAYROLL-FILE-STATUS     PIC 9(02) VALUE ZERO.
       01  STATS-FILE-STATUS               PIC 9(02) VALUE ZERO.
       01  DATA-REMAINS-SWITCH             PIC X(02) VALUE SPACES.
           88 NO-MORE-DATA                 VALUE 'NO'.
       01  ANNUAL-AMOUNT                   PIC 9(07)V99 COMP-3.
       01  TAX-AMOUNT                      PIC 9(04)V99 COMP-3.
       01  NET-AMOUNT                      PIC 9(07)V99 COMP-3.
       01  TOTAL-ANNUAL                    PIC 9(07)V99 COMP-3 VALUE 0.
       01  TOTAL-TAXES                     PIC 9(07)V99 COMP-3 VALUE 0.
       01  TOTAL-NET                       PIC 9(07)V99 COMP-3 VALUE 0.
       01  COUNTERS.
           05 TOTAL-CNT                    PIC 9(02) VALUE 0.
           05 VALID-CNT                    PIC 9(02) VALUE 0.
           05 INVALID-CNT                  PIC 9(02) VALUE 0.
       01  PRINT-DEC                       PIC Z.ZZZ.ZZ9,99.
       01  DATE-FULL.
           05 DAY-DATE                     PIC 9(02).
           05 FILLER                       PIC X(01) VALUE '-'.
           05 MONTH-DATE                   PIC 9(02).
           05 FILLER                       PIC X(01) VALUE '-'.
           05 YEAR-DATE                    PIC 9(04).
       01  VALID-PAGE.
           05 VALID-PAGE-STR               PIC X(06) VALUE 'PAGE: '.
           05 VALID-PAGE-CNT               PIC 9(02) VALUE 0.
       01  INVALID-PAGE.
           05 INVALID-PAGE-STR             PIC X(06) VALUE 'PAGE: '.
           05 INVALID-PAGE-CNT             PIC 9(02) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT PAYROLL-FILE
               OUTPUT VALID-PAYROLL-FILE
                      INVALID-PAYROLL-FILE
                      STATS-FILE

           IF PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' PAYROLL-FILE-STATUS
               PERFORM FINISH.
           IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: VALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' VALID-PAYROLL-FILE-STATUS
               PERFORM FINISH.
           IF INVALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY
                     '***ERROR OPENING INPUT FILE: INVALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' INVALID-PAYROLL-FILE-STATUS
               PERFORM FINISH.
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           PERFORM DATE-DATA
           PERFORM HEADER-VALID
           PERFORM HEADER-INVALID
           READ PAYROLL-FILE
                AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.

           PERFORM PROCESS-PAYROLLS
               UNTIL NO-MORE-DATA.
       FINISH.
           PERFORM WRITE-STATS
           DISPLAY 'TOTAL RECORDS:   '        TOTAL-CNT
           DISPLAY 'VALID RECORDS:   '        VALID-CNT
           DISPLAY 'INVALID RECORDS: '        INVALID-CNT
           DISPLAY '------------------------------------'
           MOVE TOTAL-ANNUAL TO PRINT-DEC
           DISPLAY 'TOTAL-ANNUAL-AMOUNTS:  '  PRINT-DEC
           MOVE TOTAL-TAXES TO PRINT-DEC
           DISPLAY 'TOTAL-TAXES:           '  PRINT-DEC
           MOVE TOTAL-NET TO PRINT-DEC
           DISPLAY 'TOTAL-PAYABLE-AMOUNTS: '  PRINT-DEC
           CLOSE PAYROLL-FILE
                 VALID-PAYROLL-FILE
                 INVALID-PAYROLL-FILE
                 STATS-FILE.
           IF INVALID-CNT > 0 THEN
              MOVE 55 TO RETURN-CODE
           END-IF.
            STOP RUN.

       HEADER-VALID.
           ADD 1 TO VALID-PAGE-CNT
           MOVE ALL SPACES       TO VALID-LINE
           IF VALID-PAGE-CNT NOT = 1 THEN
               MOVE X'0D0A'      TO VALID-CRLF
               WRITE VALID-LINE
               IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
                   DISPLAY
                       '***ERROR OPENING INPUT FILE: VALID-PAYROLL-FILE'
                   DISPLAY 'STATUS-CODE=' VALID-PAYROLL-FILE-STATUS
                   PERFORM FINISH
               END-IF
           END-IF.
           MOVE 'NAME'           TO VALID-LINE(004:04)
           MOVE 'HOURS'          TO VALID-LINE(021:05)
           MOVE 'HOUR-RATE'      TO VALID-LINE(033:09)
           MOVE 'AFM'            TO VALID-LINE(053:03)
           MOVE 'ANNUAL-AMOUNT'  TO VALID-LINE(070:14)
           MOVE 'TAXES'          TO VALID-LINE(092:05)
           MOVE 'NET-AMOUNT'     TO VALID-LINE(112:11)
           MOVE 'REASON'         TO VALID-LINE(132:06)
           MOVE DATE-FULL        TO VALID-LINE(148:10)
           MOVE VALID-PAGE       TO VALID-LINE(168:08)
           MOVE X'0D0A'          TO VALID-CRLF
           WRITE VALID-LINE

           IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: VALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' VALID-PAYROLL-FILE-STATUS
               PERFORM FINISH.

           MOVE ALL '-' TO VALID-LINE
           MOVE X'0D0A' TO VALID-CRLF
           WRITE VALID-LINE

           IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: VALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' VALID-PAYROLL-FILE-STATUS
               PERFORM FINISH.

       HEADER-INVALID.
           ADD 1 TO INVALID-PAGE-CNT
           MOVE ALL SPACES       TO INVALID-LINE
           IF INVALID-PAGE-CNT NOT = 1 THEN
               MOVE X'0D0A'      TO INVALID-CRLF
               WRITE INVALID-LINE
               IF INVALID-PAYROLL-FILE-STATUS NOT = 0 THEN
                   DISPLAY
                     '***ERROR OPENING INPUT FILE: INVALID-PAYROLL-FILE'
                   DISPLAY 'STATUS-CODE=' INVALID-PAYROLL-FILE-STATUS
                   PERFORM FINISH
               END-IF
           END-IF.

           MOVE 'NAME'           TO INVALID-LINE(004:04)
           MOVE 'HOURS'          TO INVALID-LINE(021:05)
           MOVE 'HOUR-RATE'      TO INVALID-LINE(033:09)
           MOVE 'AFM'            TO INVALID-LINE(053:03)
           MOVE 'ANNUAL-AMOUNT'  TO INVALID-LINE(070:14)
           MOVE 'TAXES'          TO INVALID-LINE(092:05)
           MOVE 'NET-AMOUNT'     TO INVALID-LINE(112:11)
           MOVE 'REASON'         TO INVALID-LINE(132:06)
           MOVE DATE-FULL        TO INVALID-LINE(148:10)
           MOVE INVALID-PAGE     TO INVALID-LINE(168:08)
           MOVE X'0D0A'          TO INVALID-CRLF
           WRITE INVALID-LINE

           IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY
                     '***ERROR OPENING INPUT FILE: INVALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' INVALID-PAYROLL-FILE-STATUS
               PERFORM FINISH.

           MOVE ALL '-' TO INVALID-LINE
           MOVE X'0D0A' TO INVALID-CRLF
           WRITE INVALID-LINE

           IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY
                     '***ERROR OPENING INPUT FILE: INVALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' INVALID-PAYROLL-FILE-STATUS
               PERFORM FINISH.

       DATE-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO YEAR-DATE
           MOVE FUNCTION CURRENT-DATE(5:2) TO MONTH-DATE
           MOVE FUNCTION CURRENT-DATE(7:2) TO DAY-DATE.

       PROCESS-PAYROLLS.
           ADD 1 TO TOTAL-CNT
           IF (PAYROLL-HOURS-WORKED IS NUMERIC      AND
                   PAYROLL-HOURS-WORKED NOT = ZERO) AND
              (PAYROLL-HOUR-RATE    IS NUMERIC      AND
                   PAYROLL-HOUR-RATE NOT = ZERO)    AND
              (PAYROLL-AFM          IS NUMERIC      AND
                   PAYROLL-AFM NOT = ZERO)
                PERFORM VALID-PAYROLLS
           ELSE
                PERFORM INVALID-PAYROLLS
           END-IF.

           READ PAYROLL-FILE
               AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ.
      * PROCESS-PAYROLLS-EXIT.
      *     EXIT.

       VALID-PAYROLLS.
           ADD 1 TO VALID-CNT
           COMPUTE ANNUAL-AMOUNT ROUNDED = PAYROLL-HOURS-WORKED *
                                            PAYROLL-HOUR-RATE * 14
           EVALUATE TRUE
               WHEN ANNUAL-AMOUNT <= 10000
                   MOVE ZERO TO TAX-AMOUNT
               WHEN ANNUAL-AMOUNT > 10000 AND ANNUAL-AMOUNT <= 20000
                   COMPUTE TAX-AMOUNT ROUNDED =
                   (ANNUAL-AMOUNT - 10000) * 0,10
               WHEN ANNUAL-AMOUNT > 20000
                   COMPUTE TAX-AMOUNT ROUNDED =
                   ((ANNUAL-AMOUNT - 20000) * 0,25) + 1000
           END-EVALUATE
           COMPUTE NET-AMOUNT = ANNUAL-AMOUNT - TAX-AMOUNT
           ADD ANNUAL-AMOUNT TO TOTAL-ANNUAL
           ADD TAX-AMOUNT    TO TOTAL-TAXES
           ADD NET-AMOUNT    TO TOTAL-NET

           MOVE ALL SPACES            TO VALID-LINE
           MOVE PAYROLL-EMPLOYEE-NAME TO VALID-EMPLOYEE-NAME
           MOVE PAYROLL-HOURS-WORKED  TO VALID-HOURS-WORKED
           MOVE PAYROLL-HOUR-RATE     TO VALID-HOUR-RATE
           MOVE PAYROLL-AFM           TO VALID-AFM
           MOVE ANNUAL-AMOUNT         TO VALID-ANNUAL-AMOUNT
           MOVE TAX-AMOUNT            TO VALID-TAX
           MOVE NET-AMOUNT            TO VALID-NET-AMOUNT
           MOVE X'0D0A'               TO VALID-CRLF
           WRITE VALID-LINE
           IF VALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: VALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' VALID-PAYROLL-FILE-STATUS
               PERFORM FINISH
           END-IF
           IF FUNCTION REM(VALID-CNT, 10) = 0 THEN
               PERFORM HEADER-VALID
           END-IF.

       INVALID-PAYROLLS.
           ADD 1 TO INVALID-CNT
           COMPUTE ANNUAL-AMOUNT ROUNDED = PAYROLL-HOURS-WORKED *
                                            PAYROLL-HOUR-RATE * 14
           EVALUATE TRUE
               WHEN ANNUAL-AMOUNT <= 10000
                   MOVE ZERO TO TAX-AMOUNT
               WHEN ANNUAL-AMOUNT > 10000 AND ANNUAL-AMOUNT <= 20000
                   COMPUTE TAX-AMOUNT ROUNDED =
                   (ANNUAL-AMOUNT - 10000) * 0,10
               WHEN OTHER
                   COMPUTE TAX-AMOUNT ROUNDED =
                   ((ANNUAL-AMOUNT - 20000) * 0,25) + 1000
           END-EVALUATE
           COMPUTE NET-AMOUNT = ANNUAL-AMOUNT - TAX-AMOUNT

           MOVE ALL SPACES            TO INVALID-LINE
           EVALUATE TRUE
               WHEN PAYROLL-HOURS-WORKED IS NOT NUMERIC
                   MOVE 'HOURS ARE NOT NUMERIC!!!'    TO INVALID-REASON
               WHEN PAYROLL-HOUR-RATE    IS NOT NUMERIC
                   MOVE 'HOUR-RATE IS NOT NUMERIC!!!' TO INVALID-REASON
               WHEN PAYROLL-AFM          IS NOT NUMERIC
                   MOVE 'AFM IS NOT NUMERIC!!!'       TO INVALID-REASON
               WHEN PAYROLL-HOURS-WORKED IS ZERO
                   MOVE 'HOURS ARE 0!!!'              TO INVALID-REASON
               WHEN PAYROLL-HOUR-RATE    IS ZERO
                   MOVE 'HOUR-RATE IS 0!!!'           TO INVALID-REASON
               WHEN PAYROLL-AFM          IS ZERO
                   MOVE 'AFM IS 0!!!'                 TO INVALID-REASON
               WHEN OTHER
                   MOVE 'AFM IS NOT VALID!!!'         TO INVALID-REASON
           END-EVALUATE

           MOVE PAYROLL-EMPLOYEE-NAME TO INVALID-EMPLOYEE-NAME
           MOVE PAYROLL-HOURS-WORKED  TO INVALID-HOURS-WORKED
           MOVE PAYROLL-HOUR-RATE     TO INVALID-HOUR-RATE
           MOVE PAYROLL-AFM           TO INVALID-AFM
           MOVE ANNUAL-AMOUNT         TO INVALID-ANNUAL-AMOUNT
           MOVE TAX-AMOUNT            TO INVALID-TAX
           MOVE NET-AMOUNT            TO INVALID-NET-AMOUNT
           MOVE X'0D0A'               TO INVALID-CRLF
           WRITE INVALID-LINE

           IF INVALID-PAYROLL-FILE-STATUS NOT = 0 THEN
               DISPLAY
                     '***ERROR OPENING INPUT FILE: INVALID-PAYROLL-FILE'
               DISPLAY 'STATUS-CODE=' INVALID-PAYROLL-FILE-STATUS
               PERFORM FINISH
           IF FUNCTION REM(INVALID-CNT, 10) = 0 THEN
               PERFORM HEADER-INVALID
           END-IF.

       WRITE-STATS.
           MOVE 'TOTAL-PAYROLLS: '         TO STATS-RECORD
           MOVE TOTAL-CNT                  TO STATS-VALUE
           MOVE X'0D0A'                    TO STATS-CRLF
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           MOVE 'VALID-PAYROLLS: '         TO STATS-RECORD
           MOVE VALID-CNT                  TO STATS-VALUE
           MOVE X'0D0A'                    TO STATS-CRLF
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           MOVE 'INVALID-PAYROLLS: '       TO STATS-RECORD
           MOVE INVALID-CNT                TO STATS-VALUE
           MOVE X'0D0A'                    TO STATS-CRLF
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           MOVE ALL '-'                    TO STATS-LINE
           MOVE X'0D0A'                    TO STATS-CRLF
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           MOVE 'TOTAL-ANNUAL-AMOUNTS: '   TO STATS-RECORD
           MOVE TOTAL-ANNUAL               TO STATS-VALUE
           MOVE X'0D0A'                    TO STATS-CRLF
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           MOVE 'TOTAL-TAXES: '            TO STATS-RECORD
           MOVE TOTAL-TAXES                TO STATS-VALUE
           MOVE X'0D0A'                    TO STATS-CRLF
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
           MOVE 'TOTAL-PAYABLE-AMOUNTS: '  TO STATS-RECORD
           MOVE TOTAL-NET                  TO STATS-VALUE
           WRITE STATS-LINE
           IF STATS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: STATS-FILE'
               DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
               PERFORM FINISH.
       END PROGRAM PAYROLLS.
