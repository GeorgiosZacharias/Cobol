       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFMSUBRTX.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  IDX                          PIC 99   VALUE 0.
       01  SUM-DIGITS                   PIC 9(5) VALUE 0.
       01  MULTIPLIER                   PIC 9    VALUE 8.
       01  YPOL                         PIC 99.
       01  COUNT-NUMBERS                PIC 99   VALUE 0.
       01  COUNT-CHARS                  PIC 99   VALUE 0.
       01  COUNT-SPACES                 PIC 99   VALUE 0.
       01  ERROR-SW                     PIC  9   VALUE 0.


       LINKAGE SECTION.
       01  AFM-IN                       PIC X(9).
       01  AFM-ARRAY REDEFINES AFM-IN.
           05 AFM-DIGIT OCCURS 9 TIMES  PIC X.
       01  AFM-STATUS                   PIC X(010).
       01  AFM-CAUSE                    PIC X(042).

       PROCEDURE DIVISION USING AFM-IN AFM-STATUS AFM-CAUSE.
           MOVE 0 TO COUNT-NUMBERS
           MOVE 0 TO COUNT-CHARS
           MOVE 0 TO COUNT-SPACES
           MOVE 0 TO ERROR-SW
           INSPECT AFM-IN REPLACING ALL X'0D' BY SPACES
           INSPECT AFM-IN REPLACING ALL X'0A' BY SPACES
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 9
               EVALUATE TRUE
                 WHEN  AFM-DIGIT(IDX) IS NUMERIC
                   ADD 1 TO COUNT-NUMBERS
                 WHEN  AFM-DIGIT(IDX) = SPACES
                   ADD 1 TO COUNT-SPACES
                 WHEN OTHER
                   ADD 1 TO COUNT-CHARS
               END-EVALUATE
            END-PERFORM

           EVALUATE TRUE
             WHEN AFM-IN = ALL ZEROES
                  MOVE '*** AFM ZERO IS NOT ALLOWED!!!' TO AFM-CAUSE
                  MOVE 1 TO ERROR-SW
            WHEN AFM-IN = ALL SPACES
                  MOVE '*** AFM SPACES IS NOT ALLOWED!!!' TO AFM-CAUSE
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS = 9
                  CONTINUE
            WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  > 0  AND
                  COUNT-SPACES > 0
                  MOVE '*** NO CHARS AND SPACES ALLOWED!!!' TO AFM-CAUSE
                  MOVE 1 TO ERROR-SW
            WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  = 0  AND
                  COUNT-SPACES > 0
                  MOVE '*** NO SPACES ARE ALLOWED!!!' TO AFM-CAUSE
                  MOVE 1 TO ERROR-SW
            WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  > 0  AND
                  COUNT-SPACES = 0
                  MOVE '*** NO CHARS ARE ALLOWED!!!' TO AFM-CAUSE
                  MOVE 1 TO ERROR-SW
           END-EVALUATE.
           IF ERROR-SW = 1
              MOVE 'INVALID' TO AFM-STATUS
              GOBACK
           END-IF.
           MOVE 0 TO SUM-DIGITS
           PERFORM CALCULATE-VALIDATION
           COMPUTE YPOL = FUNCTION MOD(SUM-DIGITS,11)
           IF YPOL = 10
               MOVE 0 TO YPOL
           END-IF.
           IF YPOL = FUNCTION NUMVAL(AFM-DIGIT(9))
              MOVE 'VALID'    TO AFM-STATUS
           ELSE
              MOVE 'INVALID'  TO AFM-STATUS
              MOVE 'AFM CHECK DIGIT ERROR!!!' TO AFM-CAUSE
           END-IF.
           GOBACK.

       CALCULATE-VALIDATION.
           MOVE 8 TO MULTIPLIER
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8
               COMPUTE SUM-DIGITS =
                SUM-DIGITS + FUNCTION NUMVAL(AFM-DIGIT(IDX)) *
                                            (2 ** MULTIPLIER)
               SUBTRACT 1 FROM MULTIPLIER
           END-PERFORM.
       END PROGRAM AFMSUBRTX.
