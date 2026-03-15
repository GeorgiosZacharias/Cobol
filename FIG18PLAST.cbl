       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. FIG18.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILEN ASSIGN TO DISK
             FILE STATUS IS CARD-FILE-STATUS.

           SELECT PRINT-FILE ASSIGN TO DISK
            FILE STATUS IS PRINT-FILE-STATUS.

           SELECT EXCP-FILE ASSIGN TO DISK
            FILE STATUS IS EXCP-FILE-STATUS.

           SELECT STATS-FILE ASSIGN TO DISK
            FILE STATUS IS STATS-FILE-STATUS.

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
           05  CARD-MAJOR          PIC X(012).
           05  CRLF                PIC X(002).

       FD  PRINT-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS PRINT-LINE.
       01  PRINT-LINE.
           05  PRINT-NAME          PIC X(025).
           05  FILLER              PIC X(015).
           05  PRINT-CREDITS       PIC X(010).
           05  FILLER              PIC X(015).
           05  PRINT-MAJOR         PIC X(012).
           05  FILLER              PIC X(020).
           05  PRINT-DATE-LIT      PIC X(005).
           05  PRINT-CURRENT-DATE  PIC X(017).
           05  PAGE-NUMBER         PIC X(012).
           05  PRINT-CRLF          PIC X(002).

       FD  EXCP-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS PRINT-LINE-EXCP.
       01  PRINT-LINE-EXCP.
           05  PRINT-NAME-EXCP     PIC X(025).
           05  FILLER              PIC X(015).
           05  PRINT-CREDITS-EXCP  PIC X(010).
           05  FILLER              PIC X(015).
           05  PRINT-MAJOR-EXCP    PIC X(012).
           05  FILLER              PIC X(020).
           05  EXCP-DATE-LIT       PIC X(005).
           05  EXCP-CURRENT-DATE   PIC X(017).
           05  PAGE-NUMBER-EXCP    PIC X(012).
           05  PRINT-CRLF-EXCP     PIC X(002).

       FD  STATS-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 70 CHARACTERS
           DATA RECORD IS STATS-LINE.
       01  STATS-LINE.
           05  FEATURE             PIC X(025).
           05  FILLER              PIC X(015).
           05  RECORDSSIZE         PIC X(013).
           05  FILLER              PIC X(015).
           05  PRINT-CRLF-S        PIC X(002).

       WORKING-STORAGE SECTION.

       01  CARD-FILE-STATUS        PIC 99.
       01  PRINT-FILE-STATUS       PIC 99.
       01  EXCP-FILE-STATUS        PIC 99.
       01  STATS-FILE-STATUS       PIC 99.
       01  DATA-REMAINS-SWITCH     PIC X(2)    VALUE SPACES.
           88 NO-MORE-DATA                     VALUE 'NO'.
       01  RECS-READ               PIC 99      VALUE 0.
       01  RECS-WRITTEN            PIC 99      VALUE 0.
       01  RECS-NOT-CHOSEN         PIC 99      VALUE 0.
       01  COUNT-10-W              PIC 99      VALUE 0.
       01  COUNT-10-NW             PIC 99      VALUE 0.
       01  PAGE-COUNT-W            PIC 99      VALUE 0.
       01  PAGE-COUNT-NW           PIC 99      VALUE 0.
       01  SEPARATE-LINE.
           05 SEPARATE-DASHES      PIC X(131).
           05 SEPARATE-CRLF        PIC X(002)  VALUE X'0D0A'.
       01  MAX                     PIC 9(003)  VALUE 0.
       01  MIN                     PIC 9(003)  VALUE 999.
       01  SUM-CREDITS             PIC 9(005)  VALUE 0.
       01  AVG                     PIC 9(003)  VALUE 0.
       01  DOCTOR                  PIC 9(002)  VALUE 0.
       01  CHEMIST                 PIC 9(002)  VALUE 0.
       01  NUCPHY                  PIC 9(002)  VALUE 0.
       01  ARCHITECT               PIC 9(002)  VALUE 0.
       01  IT                      PIC 9(002)  VALUE 0.
       01  OTHER-CNT               PIC 9(002)  VALUE 0.
       01  DOCTOR-LIT              PIC X(012)  VALUE 'DOCTOR'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       MAINLINE.

           OPEN INPUT CARD-FILEN, OUTPUT PRINT-FILE, EXCP-FILE,
                                        STATS-FILE.
           IF CARD-FILE-STATUS NOT = 0
              DISPLAY '***ERROR OPENING INPUT FILE:CARD-FILE!!!'
              DISPLAY 'STATUS-CODE=' CARD-FILE-STATUS
              GO TO STOPRUN.
           IF PRINT-FILE-STATUS NOT = 0
              DISPLAY '***ERROR OPENING OUTPUT FILE:PRINT-FILE!!!'
              DISPLAY 'STATUS-CODE=' PRINT-FILE-STATUS
              GO TO STOPRUN.
           IF EXCP-FILE-STATUS NOT = 0
              DISPLAY '***ERROR OPENING INPUT FILE:EXCP-FILE!!!'
              DISPLAY 'STATUS-CODE=' EXCP-FILE-STATUS
              GO TO STOPRUN.
           IF STATS-FILE-STATUS NOT = 0
              DISPLAY '***ERROR OPENING OUTPUT FILE:STAS-FILE!!!'
              DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
              GO TO STOPRUN.

           READ CARD-FILEN
             AT END MOVE 'NO'   TO DATA-REMAINS-SWITCH.

           PERFORM PROCESS-CARDS THRU PROCESS-CARDS-EXIT
             UNTIL NO-MORE-DATA.

           PERFORM PRINT-STATS-PARAGRAPH.

       STOPRUN.
           CLOSE CARD-FILEN, PRINT-FILE, EXCP-FILE, STATS-FILE.
           DISPLAY 'RECORDS READ       = ' RECS-READ.
           DISPLAY 'RECORDS WRITTEN    = ' RECS-WRITTEN.
           DISPLAY 'RECORDS NOT CHOSEN = ' RECS-NOT-CHOSEN.
           DISPLAY '-----------------------'.
           DISPLAY 'MAX-CREDITS        = ' MAX.
           DISPLAY 'MIN-CREDITS        = ' MIN.
           DISPLAY 'AVG-CREDITS        = ' AVG.
           DISPLAY '-----------------------'.
           DISPLAY 'DOCTOR             = ' DOCTOR.
           DISPLAY 'CHEMIST            = ' CHEMIST.
           DISPLAY 'NUCPHY             = ' NUCPHY.
           DISPLAY 'ARCHITECT          = ' ARCHITECT.
           DISPLAY 'IT                 = ' IT.
           DISPLAY 'OTHER              = ' OTHER-CNT.
           IF OTHER-CNT >0 THEN
               DISPLAY '*** WARNING!!! INPUT FILE CONTAINS:' OTHER-CNT
                   ' NEW PREOFESSIONS...'
           END-IF.
           STOP RUN.

       PROCESS-CARDS.
           ADD 1 TO RECS-READ.

           IF (CARD-CREDITS  >= 100 )         OR
              (CARD-CREDITS  >= 80            AND
               CARD-MAJOR     = 'DOCTOR')     OR
              (CARD-CREDITS  >= 90            AND
               CARD-MAJOR     = 'ARCHITECT')  OR
              (CARD-MAJOR     = 'NUCPHY')     THEN
      *     IF (CARD-CREDITS > 115)       OR
      *        (CARD-MAJOR   = 'NUCPHY')   OR
      *        (CARD-CREDITS > 100         AND
      *        (CARD-MAJOR   = 'ARCHITECT' OR = 'DOCTOR')) OR
      *        (CARD-MAJOR   = 'IT'        AND  CARD-CREDITS >= 109)
              PERFORM PROCESS-SELECTED-RECORDS
              IF MAX < CARD-CREDITS THEN
                 MOVE CARD-CREDITS TO MAX
              END-IF
              IF MIN > CARD-CREDITS THEN
                 MOVE CARD-CREDITS TO MIN
              END-IF

              ADD CARD-CREDITS TO SUM-CREDITS

      *        PERFORM CALCULATE-PRPOFESSION
              PERFORM EVALUATE-PROFESSIONS

           ELSE

              PERFORM PROCESS-NOT-SELECTED-RECORDS
              IF MAX < CARD-CREDITS THEN
                 MOVE CARD-CREDITS TO MAX
              END-IF
              IF MIN > CARD-CREDITS THEN
                 MOVE CARD-CREDITS TO MIN
              END-IF
              ADD CARD-CREDITS TO SUM-CREDITS

      *        PERFORM CALCULATE-PRPOFESSION
              PERFORM EVALUATE-PROFESSIONS

           END-IF

           READ CARD-FILEN
                AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.

       PROCESS-CARDS-EXIT.
           EXIT.

       PROCESS-SELECTED-RECORDS.
           ADD   1  TO   RECS-WRITTEN
           ADD   1  TO   COUNT-10-W

           IF COUNT-10-W = 1 THEN
              ADD 1  TO   PAGE-COUNT-W
              MOVE SPACES           TO   PRINT-LINE
              MOVE 'NAME'           TO   PRINT-NAME
              MOVE 'CREDITS'        TO   PRINT-CREDITS
              MOVE 'PROFESSION'     TO   PRINT-MAJOR

              MOVE 'DATE:'          TO   PRINT-DATE-LIT
              MOVE FUNCTION
                CURRENT-DATE(7:2)   TO   PRINT-CURRENT-DATE(1:2)
              MOVE '-' TO PRINT-CURRENT-DATE(3:1)
              MOVE FUNCTION
                CURRENT-DATE(5:2)   TO   PRINT-CURRENT-DATE(4:2)
              MOVE '-' TO PRINT-CURRENT-DATE(6:1)
              MOVE FUNCTION
                CURRENT-DATE(1:4)   TO   PRINT-CURRENT-DATE(7:4)

              STRING 'PAGE NO.:'  DELIMITED BY SIZE
                     PAGE-COUNT-W DELIMITED BY SIZE
                     INTO PAGE-NUMBER
              END-STRING

              MOVE X'0D0A'           TO   PRINT-CRLF
              WRITE PRINT-LINE

              MOVE SPACES            TO   PRINT-LINE
              MOVE ALL '-'           TO   SEPARATE-DASHES(1:77)
              MOVE X'0D0A'           TO   SEPARATE-CRLF
              WRITE PRINT-LINE FROM SEPARATE-LINE
           END-IF

           MOVE SPACES       TO   PRINT-LINE
           MOVE CARD-NAME    TO   PRINT-NAME
           MOVE CARD-CREDITS TO   PRINT-CREDITS

           MOVE CARD-MAJOR   TO   PRINT-MAJOR
           MOVE X'0D0A'      TO   PRINT-CRLF
           WRITE PRINT-LINE
           IF PRINT-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING OUTPUT FILE:PRINT-FILE!!!'
              DISPLAY 'STATUS-CODE=' PRINT-FILE-STATUS
              GO TO STOPRUN
           END-IF

           IF COUNT-10-W = 5 THEN
              MOVE 0       TO  COUNT-10-W
              MOVE SPACES  TO  PRINT-LINE
              MOVE X'0D0A' TO  PRINT-CRLF
              WRITE PRINT-LINE
      *          WRITE PRINT-LINE AFTER ADVANCING
      *                                        2 LINE
           END-IF .

       PROCESS-NOT-SELECTED-RECORDS.
           ADD 1  TO   RECS-NOT-CHOSEN
           ADD 1  TO   COUNT-10-NW

           IF COUNT-10-NW = 1 THEN
              ADD 1 TO PAGE-COUNT-NW
              MOVE SPACES          TO   PRINT-LINE-EXCP
              MOVE 'NAME'          TO   PRINT-NAME-EXCP
              MOVE 'CREDITS'       TO   PRINT-CREDITS-EXCP
              MOVE 'PROFESSION'    TO   PRINT-MAJOR-EXCP
              MOVE 'DATE:'         TO   EXCP-DATE-LIT
              MOVE FUNCTION
                CURRENT-DATE(7:2)   TO   EXCP-CURRENT-DATE(1:2)
              MOVE '-' TO EXCP-CURRENT-DATE(3:1)
              MOVE FUNCTION
                CURRENT-DATE(5:2)   TO   EXCP-CURRENT-DATE(4:2)
              MOVE '-' TO EXCP-CURRENT-DATE(6:1)
              MOVE FUNCTION
                CURRENT-DATE(1:4)   TO   EXCP-CURRENT-DATE(7:4)

              STRING 'PAGE NO.:'   DELIMITED BY SIZE
                     PAGE-COUNT-NW DELIMITED BY SIZE
                     INTO PAGE-NUMBER-EXCP
              END-STRING
              MOVE X'0D0A'         TO   PRINT-CRLF-EXCP
              WRITE PRINT-LINE-EXCP

              MOVE SPACES          TO   PRINT-LINE-EXCP
              MOVE ALL '-'         TO   SEPARATE-DASHES(1:77)
              MOVE X'0D0A'         TO   SEPARATE-CRLF
              WRITE PRINT-LINE-EXCP FROM SEPARATE-LINE
           END-IF

           MOVE SPACES       TO   PRINT-LINE-EXCP
           MOVE CARD-NAME    TO   PRINT-NAME-EXCP
           MOVE CARD-CREDITS TO   PRINT-CREDITS-EXCP
           MOVE CARD-MAJOR   TO   PRINT-MAJOR-EXCP
           MOVE X'0D0A'      TO   PRINT-CRLF-EXCP
           WRITE PRINT-LINE-EXCP
           IF EXCP-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING OUTPUT FILE:EXCP-FILE!!!'
              DISPLAY 'STATUS-CODE=' EXCP-FILE-STATUS
              GO TO STOPRUN
           END-IF

           IF COUNT-10-NW = 5 THEN
              MOVE 0       TO  COUNT-10-NW
              MOVE SPACES  TO  PRINT-LINE-EXCP
              MOVE X'0D0A' TO  PRINT-CRLF-EXCP
              WRITE PRINT-LINE-EXCP
           END-IF.

       PRINT-STATS-PARAGRAPH.
           MOVE SPACES               TO  STATS-LINE
           MOVE 'RECORDS READ'       TO  FEATURE
           MOVE RECS-READ            TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'RECORDS WRITTEN'    TO  FEATURE
           MOVE RECS-WRITTEN         TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'RECORDS NOT CHOSEN' TO  FEATURE
           MOVE RECS-NOT-CHOSEN      TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE ALL '-'             TO   STATS-LINE
           MOVE X'0D0A'             TO   PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'MAX-CREDITS'        TO  FEATURE
           MOVE MAX                  TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'MIN-CREDITS'        TO  FEATURE
           MOVE MIN                  TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'AVG-CREDITS'        TO  FEATURE
           DIVIDE SUM-CREDITS BY RECS-READ GIVING AVG
           MOVE AVG                  TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE ALL '-'             TO   STATS-LINE
           MOVE X'0D0A'             TO   PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'DOCTOR'             TO  FEATURE
           MOVE DOCTOR               TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'CHEMIST'            TO  FEATURE
           MOVE CHEMIST              TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'NUCPHY'             TO  FEATURE
           MOVE NUCPHY               TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'ARCHITECT'          TO  FEATURE
           MOVE ARCHITECT            TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE SPACES               TO  STATS-LINE
           MOVE 'IT'                 TO  FEATURE
           MOVE IT                   TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE
           MOVE 'OTHER'              TO  FEATURE
           MOVE OTHER-CNT            TO  RECORDSSIZE
           MOVE X'0D0A'              TO  PRINT-CRLF-S
           WRITE STATS-LINE

           IF STATS-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING OUTPUT FILE:STAS-FILE!!!'
              DISPLAY 'STATUS-CODE=' STATS-FILE-STATUS
              GO TO STOPRUN
           END-IF.

      * CALCULATE-SECTION SECTION.
       CALCULATE-PRPOFESSION.
           IF CARD-MAJOR = 'DOCTOR'   THEN
              ADD  1  TO  DOCTOR
           ELSE
              IF CARD-MAJOR = 'CHEMIST'   THEN
                 ADD  1  TO  CHEMIST
              ELSE
                 IF CARD-MAJOR = 'NUCPHY'    THEN
                    ADD  1  TO  NUCPHY
                 ELSE
                    IF CARD-MAJOR = 'ARCHITECT' THEN
                       ADD  1  TO  ARCHITECT
                    ELSE
                    IF CARD-MAJOR = 'IT'        THEN
                       ADD  1  TO  IT
                    END-IF.
       EVALUATE-PROFESSIONS.
      *     DISPLAY '------EVALUATE--------'
           EVALUATE CARD-MAJOR
             WHEN 'DOCTOR'
               ADD 1 TO DOCTOR
             WHEN 'CHEMIST'
               ADD 1 TO CHEMIST
             WHEN 'NUCPHY'
               ADD  1  TO  NUCPHY
              WHEN 'ARCHITECT'
               ADD  1  TO  ARCHITECT
              WHEN 'IT'
               ADD  1  TO  IT
              WHEN OTHER
               ADD 1 TO OTHER-CNT
           END-EVALUATE.
       PROCESS-FILE-A.
           DISPLAY '*** PROCESS-FILE-A'.
       PROCESS-FILE-A-EXIT.
           EXIT.
       END PROGRAM FIG18.
