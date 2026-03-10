       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. FIG18.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILEN ASSIGN TO DISK
             FILE STATUS IS CARD-FILEN-STATUS.

           SELECT PRINT-FILE ASSIGN TO DISK
             FILE STATUS IS PRINT-FILE-STATUS.

           SELECT EXCP-FILE ASSIGN TO DISK
             FILE STATUS IS EXCP-FILE-STATUS.
           SELECT STATS-FILE ASSIGN TO DISK
             FILE STATUS IS STATS-FILE.
       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILEN
      *     LABEL RECORD IS OMITTED
           RECORD CONTAINS 44 CHARACTERS
           DATA RECORD IS CARD-IN.
       01  CARD-IN.
           05  CARD-NAME           PIC A(025).
           05  FILLER              PIC X(001).
           05  CARD-CREDITS        PIC 9(003).
           05  FILLER              PIC X(001).
           05  CARD-MAJOR          PIC A(012).
           05  CRLF                PIC X(002).

       FD  PRINT-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 61 CHARACTERS
           DATA RECORD IS PRINT-LINE.
       01  PRINT-LINE.
           05  QUEUE-NUMBER        PIC X(003) VALUE SPACES.
           05  FILLER-1            PIC X(004).
           05  PRINT-NAME          PIC X(025).
           05  FILLER-2            PIC X(004).
           05  PRINT-CREDITS       PIC X(010) VALUE SPACES.
           05  FILLER-3            PIC X(001).
           05  PRINT-PROFESSION    PIC A(012).
           05  PRINT-CRLF          PIC X(002).
       FD  EXCP-FILE                                                    -
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 61 CHARACTERS
           DATA RECORD IS EXCP-PRINT-LINE.
       01  EXCP-PRINT-LINE.
           05  QUEUE-NUMBER-CP        PIC X(003) VALUE SPACES.
           05  FILLER-1-CP            PIC X(004).
           05  PRINT-NAME-CP          PIC X(025).
           05  FILLER-2-CP            PIC X(004).
           05  PRINT-CREDITS-CP       PIC X(010) VALUE SPACES.
           05  FILLER-3-CP            PIC X(001).
           05  PRINT-PROFESSION-CP    PIC A(012).
           05  PRINT-CRLF-CP          PIC X(002).

       WORKING-STORAGE SECTION.
       01  CARD-FILEN-STATUS        PIC 99.
       01  PRINT-FILE-STATUS       PIC 99.
       01  EXCP-FILE-STATUS        PIC 99.
       01  DATA-REMAINS-SWITCH     PIC X(002)    VALUE SPACES.
           88 NO-MORE-DATA         VALUE 'NO'.
       01  RECS-READ               PIC 9(002)    VALUE 0.
       01  RECS-WRITTEN            PIC 9(002)    VALUE 0.
       01  RECS-NOT-CHOSEN         PIC 9(002)    VALUE 0.
       01  ONE-DIGIT               PIC 9(001).
       01  OUTPUT-VARIABLE         PIC X(003).
       01  LINES-PER-PAGE       PIC 9 VALUE 0.
       01  PAGE-LIMIT           PIC 9 VALUE 5.

       01  CURRENT-DATES.
           05 CR-YEAR              PIC 9(4).
           05 CR-MONTH             PIC 9(2).
           05 CR-DAY               PIC 9(2).
      *returns 21 characters
           05 FILLER               PIC X(13).

       01  FULL-DATE             PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATES
           STRING CR-DAY "/" CR-MONTH "/" CR-YEAR
              DELIMITED BY SIZE
              INTO FULL-DATE
           END-STRING.
       MAINLINE.
           OPEN INPUT CARD-FILEN, OUTPUT PRINT-FILE, EXCP-FILE.
           IF CARD-FILEN-STATUS NOT = 0
              DISPLAY 'ERROR OPENING INPUT FILE:CARD-FILEN!!!'
              DISPLAY 'STATUS-CODE=' CARD-FILEN-STATUS
              GO TO FINISH.
           IF PRINT-FILE-STATUS NOT = 0
                DISPLAY '***ERROR OPENING OUTPUT FILE:PRINT-FILE!!!'
                DISPLAY 'STATUS-CODE=' PRINT-FILE-STATUS
                GO TO FINISH.
           IF EXCP-FILE-STATUS NOT = 0
                DISPLAY '***ERROR OPENING OUTPUT FILE:EXCP-FILE!!!'
                DISPLAY 'STATUS-CODE=' EXCP-FILE-STATUS
                GO TO FINISH.

           READ CARD-FILEN
                AT END MOVE 'NO'   TO DATA-REMAINS-SWITCH.
           PERFORM DATE-PRINT
           PERFORM SEPERATOR-EXCP
           PERFORM HEADER-PRINT
           PERFORM SEPERATOR-EXCP

           PERFORM DATE-PRINT-EXCP
           PERFORM SEPERATOR-PRINT-EXCP
           PERFORM HEADER-PRINT-EXCP
           PERFORM SEPERATOR-PRINT-EXCP

           PERFORM PROCESS-CARDS THRU PROCESS-CARDS-EXIT
               UNTIL NO-MORE-DATA.
       FINISH.
           CLOSE CARD-FILEN, PRINT-FILE, EXCP-FILE.
           DISPLAY '***RECORDS READ       = ' RECS-READ.
           DISPLAY '***RECORDS WRITTEN    = ' RECS-WRITTEN.
           DISPLAY '***RECORDS NOT CHOSEN = ' RECS-NOT-CHOSEN.

           STOP RUN.
       DATE-PRINT.
           MOVE SPACES TO PRINT-LINE
           MOVE 'DATE:' TO PRINT-NAME
           MOVE FULL-DATE TO PRINT-CREDITS
           MOVE X'0D0A' TO PRINT-CRLF
           WRITE PRINT-LINE.
       DATE-PRINT-EXCP.
           MOVE SPACES      TO EXCP-PRINT-LINE
           MOVE 'DATE:'     TO PRINT-NAME-CP
           MOVE FULL-DATE   TO PRINT-CREDITS-CP
           MOVE X'0D0A'     TO PRINT-CRLF-CP
           WRITE EXCP-PRINT-LINE.

       HEADER-PRINT.                                                          -
           MOVE 'A/A'        TO   QUEUE-NUMBER
           MOVE SPACES       TO   FILLER-1
           MOVE SPACES       TO   FILLER-2
           MOVE SPACES       TO   FILLER-3
           MOVE 'NAME'       TO   PRINT-NAME
           MOVE 'CREDITS'    TO   PRINT-CREDITS
           MOVE 'PROFESSION' TO   PRINT-PROFESSION
           MOVE X'0D0A'      TO   PRINT-CRLF
           WRITE PRINT-LINE.
       HEADER-PRINT-EXIT.
           EXIT.

       SEPERATOR-EXCP.
           MOVE ALL '-' TO PRINT-LINE
           MOVE X'0D0A'                     TO   PRINT-CRLF
           WRITE PRINT-LINE.

       HEADER-PRINT-EXCP.


           MOVE 'A/A'        TO   QUEUE-NUMBER-CP
           MOVE SPACES       TO   FILLER-1-CP
           MOVE SPACES       TO   FILLER-2-CP
           MOVE SPACES       TO   FILLER-3-CP
           MOVE 'NAME'       TO   PRINT-NAME-CP
           MOVE 'CREDITS'    TO   PRINT-CREDITS-CP
           MOVE 'PROFESSION' TO   PRINT-PROFESSION-CP
           MOVE X'0D0A'      TO   PRINT-CRLF-CP
           WRITE EXCP-PRINT-LINE.

       SEPERATOR-PRINT-EXCP.
           MOVE ALL '-'                     TO EXCP-PRINT-LINE
           MOVE X'0D0A'                     TO   PRINT-CRLF-CP
           WRITE EXCP-PRINT-LINE.

       PROCESS-CARDS.
       PROCESS-NEXT-RECORD.
           ADD 1 TO RECS-READ
           MOVE SPACES TO OUTPUT-VARIABLE
           IF (CARD-CREDITS  >= 100 )         OR
              (CARD-CREDITS  >= 80            AND
               CARD-MAJOR     = 'DOCTOR')     OR
              (CARD-CREDITS  >= 90            AND
               CARD-MAJOR     = 'ARCHITECT')  OR
              (CARD-MAJOR     = 'NUCPHY')     THEN
              PERFORM WRITE-SELECTED-RECORDS
           ELSE
              PERFORM WRITE-NOT-SELECTED-RECORDS
           END-IF.
           READ CARD-FILEN
                AT END MOVE 'NO'   TO DATA-REMAINS-SWITCH.

       PROCESS-CARDS-EXIT.
           EXIT.

       WRITE-SELECTED-RECORDS.
           ADD 1 TO RECS-WRITTEN
           IF (RECS-WRITTEN<5) THEN
                MOVE RECS-WRITTEN TO ONE-DIGIT
                STRING ONE-DIGIT DELIMITED BY SIZE
                       ' ' DELIMITED BY SPACE
                       '.' DELIMITED BY SIZE
                       INTO OUTPUT-VARIABLE
                END-STRING
           ELSE
                STRING RECS-WRITTEN DELIMITED BY SIZE
                       ' ' DELIMITED BY SPACE
                       '.' DELIMITED BY SIZE
                       INTO OUTPUT-VARIABLE
                END-STRING
           END-IF
           MOVE OUTPUT-VARIABLE TO   QUEUE-NUMBER
           MOVE SPACES          TO   FILLER-1
           MOVE SPACES          TO   FILLER-2
           MOVE SPACES          TO   FILLER-3
           MOVE CARD-NAME       TO   PRINT-NAME
           MOVE CARD-CREDITS    TO   PRINT-CREDITS
           MOVE CARD-MAJOR      TO   PRINT-PROFESSION
           MOVE X'0D0A'         TO   PRINT-CRLF
      *    MOVE X'0D0A'         TO   PRINT-CRLF
           WRITE PRINT-LINE
           IF PRINT-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING OUTPUT FILE: '
                      'PRINT-FILE!!!'
              DISPLAY 'STATUS-CODE=' PRINT-FILE-STATUS
              GO TO FINISH
           END-IF
           .
       WRITE-NOT-SELECTED-RECORDS.
           ADD 1 TO RECS-NOT-CHOSEN
           IF (RECS-NOT-CHOSEN<5) THEN
               MOVE RECS-NOT-CHOSEN TO ONE-DIGIT
               STRING ONE-DIGIT DELIMITED BY SIZE
                      ' ' DELIMITED BY SPACE
                      '.' DELIMITED BY SIZE
                      INTO OUTPUT-VARIABLE
                END-STRING
           ELSE
               STRING RECS-NOT-CHOSEN DELIMITED BY SIZE
                      ' ' DELIMITED BY SPACE
                      '.' DELIMITED BY SIZE
                      INTO OUTPUT-VARIABLE
               END-STRING
           END-IF
           MOVE OUTPUT-VARIABLE TO   QUEUE-NUMBER-CP
           MOVE SPACES          TO   FILLER-1-CP
           MOVE SPACES          TO   FILLER-2-CP
           MOVE SPACES          TO   FILLER-3-CP
           MOVE CARD-NAME       TO   PRINT-NAME-CP
           MOVE CARD-CREDITS    TO   PRINT-CREDITS-CP
           MOVE CARD-MAJOR      TO   PRINT-PROFESSION-CP
           MOVE X'0D0A'         TO   PRINT-CRLF-CP
           MOVE X'0D0A'         TO   PRINT-CRLF-CP
           WRITE EXCP-PRINT-LINE
           IF EXCP-FILE-STATUS NOT = 0
              DISPLAY '***ERROR OPENING OUTPUT FILE:EXCP-FILE'
              DISPLAY 'STATUS-CODE=' EXCP-FILE-STATUS
              GO TO FINISH
           END-IF
           .
       END PROGRAM FIG18.
