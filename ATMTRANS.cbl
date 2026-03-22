
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMTRANS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ATMS-FILE ASSIGN TO DISK
             FILE STATUS IS ATMS-FILE-STATUS.
       DATA DIVISION.

       FILE SECTION.
       FD  ATMS-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 37 CHARACTERS
           DATA RECORD IS ATM-TRANSACTION.
       01  ATM-TRANSACTION.
           05 REGION-IN           PIC X(20).
           05 TRN-IN              PIC 9(5).
           05 AMOUNT-IN           PIC 9(10).
           05 CRLF                PIC X(2).


       WORKING-STORAGE SECTION.
       01   ATMS-FILE-STATUS PIC 99 VALUE 0.

       01  REGION-TABLE.
           05 REGION-ENTRY OCCURS 50 TIMES.
              10 REGION-NAME       PIC X(20) VALUE SPACES.
              10 REGION-TRN        PIC 9(5)  VALUE 0.
              10 REGION-AMOUNT     PIC 9(10) VALUE 0.
       01  DATA-REMAINS-SWITCH     PIC X(002)    VALUE SPACES.
           88 NO-MORE-DATA         VALUE 'NO'.
       01  IDX                     PIC 99 VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM OPEN-FILES
           INITIALIZE REGION-TABLE
           READ ATMS-FILE
            AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.
           PERFORM PROCESS-RECORD THRU PROCESS-RECORD-EXIT
           UNTIL NO-MORE-DATA

           PERFORM DISPLAY-RESULTS

           PERFORM FINISH.

       OPEN-FILES.
      *=================================================================
           OPEN INPUT ATMS-FILE

           IF ATMS-FILE-STATUS NOT = '00'
               DISPLAY '***ERROR OPENING INPUT FILE: ATMS-FILE'
               DISPLAY 'STATUS-CODE=' ATMS-FILE-STATUS
               PERFORM FINISH
           END-IF.


      *=================================================================
       PROCESS-RECORD.
           PERFORM VARYING IDX FROM 1 BY 1
            UNTIL IDX > 50 OR
                  REGION-NAME(IDX) = REGION-IN OR
                  REGION-NAME(IDX) = SPACES
           END-PERFORM
           IF IDX > 50
                DISPLAY '***----------TABLE FULL----------***'
                DISPLAY '***----------INCREASE SIZE----------***'
                PERFORM FINISH
               ELSE
           IF REGION-NAME(IDX) = REGION-IN
                    ADD TRN-IN    TO REGION-TRN(IDX)
                    ADD AMOUNT-IN TO REGION-AMOUNT(IDX)
                ELSE
                    MOVE REGION-IN TO REGION-NAME(IDX)
                    MOVE TRN-IN    TO REGION-TRN(IDX)
                    MOVE AMOUNT-IN TO REGION-AMOUNT(IDX)
                END-IF
               END-IF
           READ ATMS-FILE
           AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.
      *=================================================================
       PROCESS-RECORD-EXIT.
           EXIT.
      *=================================================================
       DISPLAY-RESULTS.

           DISPLAY '-----------------------------'
           DISPLAY 'REGION               TRN     AMOUNT'
           DISPLAY '-----------------------------'

           PERFORM VARYING IDX FROM 1 BY 1
            UNTIL IDX > 50 OR REGION-NAME(IDX) = SPACES
                DISPLAY REGION-NAME(IDX)
                        REGION-TRN(IDX)
                        '|'
                        REGION-AMOUNT(IDX)
           END-PERFORM.
      *=================================================================
       FINISH.
           CLOSE ATMS-FILE

           STOP RUN.

       END PROGRAM ATMTRANS.
