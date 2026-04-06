
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. THREE_DIMENSION_UNI.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNIVTHESS-FILE ASSIGN TO DISK
             FILE STATUS IS UNIVTHESS-FILE-STATUS.
           SELECT TOTALS-FILE ASSIGN TO DISK
             FILE STATUS IS TOTALS-FILE-STATUS.
       DATA DIVISION.

       FILE SECTION.
       FD  UNIVTHESS-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 90 CHARACTERS
           DATA RECORD IS UNIVTHESS-IN.
       01  UNIVTHESS-IN.
           05  UNIVTHESS-NAME              PIC X(40).
           05  UNIVTHESS-MAJOR             PIC X(23).
           05  UNIVTHESS-YEAR              PIC X(22).
           05  UNIVTHESS-STUDENTS          PIC 9(3).
           05  UNIVTHESS-CRLF              PIC X(02).

       FD  TOTALS-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 86 CHARACTERS
           DATA RECORD IS UNIVTHESS-OUT.
       01  UNIVTHESS-OUT.
           05  OUTPUT-LINE                  PIC X(84).
           05  VALID-CRLF                   PIC X(02).

       WORKING-STORAGE SECTION.
       01  UNIVTHESS-FILE-STATUS           PIC 99.
       01  TOTALS-FILE-STATUS              PIC 99.
       01  UNIVERSITIES.
           05 UNIVERSITY OCCURS 4 TIMES.
               10 UNIV PIC X(14).
               10 DEPARTMENT OCCURS 4 TIMES.
                   15 DEPART PIC X(22).
                   15 YEAR OCCURS 5 TIMES PIC 9(4).
       01  DATA-REMAINS-SWITCH     PIC X(002)    VALUE SPACES.
           88 NO-MORE-DATA         VALUE 'NO'.
       01  IDX                     PIC 99 VALUE 0.
       01  IDJ                     PIC 99 VALUE 0.
       01  IDK                     PIC 99 VALUE 0.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM OPEN-FILES.
           READ UNIVTHESS-FILE
            AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.
           PERFORM PROCESS-FILE.
           PERFORM FINISH.
      *=================================================================
       OPEN-FILES.
           OPEN INPUT UNIVTHESS-FILE
                OUTPUT TOTALS-FILE.
           IF UNIVTHESS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING INPUT FILE: UNIVTHESS-FILE'
               DISPLAY 'STATUS-CODE=' UNIVTHESS-FILE-STATUS
               PERFORM FINISH
           END-IF
           IF  TOTALS-FILE-STATUS NOT = 0 THEN
               DISPLAY '***ERROR OPENING OUTPUT FILE: TOTALS-FILE'
               DISPLAY 'STATUS-CODE=' TOTALS-FILE-STATUS
               PERFORM FINISH
           END-IF.
      *=================================================================
       PROCESS-FILE.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 64 OR
                                                       NO-MORE-DATA

            READ UNIVTHESS-FILE
                AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-PERFORM.
      *=================================================================
       FINISH.
           CLOSE UNIVTHESS-FILE TOTALS-FILE
           STOP RUN.
       END PROGRAM THREE_DIMENSION_UNI.
