
       IDENTIFICATION DIVISION.

       PROGRAM-ID. 1DSORT.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
      *
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01  MYAREA              PIC X(20) VALUE '57663311442205748396'.
       01  ONE-TABLE.
           05  TABLE-POS OCCURS 10 TIMES  PIC 99 .
       01  IDI                           PIC 9(2).
       01  IDJ                           PIC 9(2).
       01  TABLE-SUM                     PIC 9(4) VALUE 0.
       01  TABLE-MAX                     PIC 9(3) VALUE 0.
       01  TABLE-MIN                     PIC 9(3) VALUE 99.
       01  TABLE-AVG                     PIC 99V9 VALUE 0.
       01  TEMP                          PIC 99 VALUE 1.
       01  TEMP-SORT                     PIC 99 VALUE 0  .
      *-----------------------
       PROCEDURE DIVISION.
      *
       MAIN-PROCEDURE.
           INITIALIZE ONE-TABLE.
           PERFORM PROCESS-TABLE.
           PERFORM CALCULATIONS-TABLE.
           PERFORM SORT-TABLE.
           PERFORM DISPLAY-SORTED.
           PERFORM FINISH.
       PROCESS-TABLE.
           DISPLAY '------------------------------------------------'
           DISPLAY '***DISPLAY INTERNAL TABLE CONTENTS ***'
           DISPLAY '------------------------------------------------'
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI>10
               MOVE MYAREA(TEMP:2) TO TABLE-POS(IDI)
               ADD 2 TO TEMP
               DISPLAY 'ENTRY' IDI '-->'  TABLE-POS(IDI)
           END-PERFORM.
       CALCULATIONS-TABLE.
           DISPLAY '------------------------------------------------'
           DISPLAY '***FIND AVERANGE, MIN & MAX NUMBERS OF ITABLE***'
           DISPLAY '------------------------------------------------'
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI>10
               ADD TABLE-POS(IDI) TO TABLE-SUM
               IF  TABLE-POS(IDI)>TABLE-MAX
                   MOVE TABLE-POS(IDI) TO TABLE-MAX
               END-IF
               IF  TABLE-POS(IDI)<TABLE-MIN
                   MOVE TABLE-POS(IDI) TO TABLE-MIN
               END-IF
           END-PERFORM
           COMPUTE TABLE-AVG = TABLE-SUM / (IDI - 1)
           DISPLAY '***AVERAGE =' TABLE-AVG
           DISPLAY '***MIN =' TABLE-MIN
           DISPLAY '***MAX =' TABLE-MAX.
       SORT-TABLE.
           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI > 9
            PERFORM VARYING IDJ FROM IDI BY 1 UNTIL IDJ > 10
                IF TABLE-POS(IDI) > TABLE-POS(IDJ)
                    MOVE TABLE-POS(IDI) TO TEMP-SORT
                    MOVE TABLE-POS(IDJ) TO TABLE-POS(IDI)
                    MOVE TEMP-SORT TO TABLE-POS(IDJ)
                END-IF
            END-PERFORM
           END-PERFORM.

       DISPLAY-SORTED.
           DISPLAY '------------------------------------------------'
           DISPLAY '***DISPLAY ITABLE(SORTED) CONTENTS ***'
           DISPLAY '------------------------------------------------'

           PERFORM VARYING IDI FROM 1 BY 1 UNTIL IDI>10
               DISPLAY 'ENTRY' IDI '-->' TABLE-POS(IDI)
           END-PERFORM.
       FINISH.
           STOP RUN.

       END PROGRAM 1DSORT.
