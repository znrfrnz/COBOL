       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-REPORT.
       AUTHOR. JC EDWARD DIRK ALECKZ GRETA.
       INSTALLATION. COMPLAB.
       DATE-WRITTEN. NOVEMBER 17, 2025.
       DATE-COMPILED. NOVEMBER 17, 2025.
       SECURITY. ACCESSIBLE TO ALL.
       REMARKS. FIRST PROGRAM EXERCISE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LAPTOP.
       OBJECT-COMPUTER. LAPTOP.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO "INFILE.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "OUTFILE.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS REC-IN.
       01  REC-IN.
           05 DEPTC-IN        PIC X.
           05 FILLER          PIC X.
           05 EMPNO-IN        PIC X(10).
           05 FILLER          PIC X.
           05 EMPNAME-IN      PIC X(25).
           05 FILLER          PIC X.
           05 ETYPE-IN        PIC X(2).
           05 FILLER          PIC X.
           05 HOURS-STR       PIC X(8).
           05 FILLER          PIC X.
           05 RATE-STR        PIC X(8).

       FD  OUTFILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OUTREC.
       01  OUTREC.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.

       01  HD1-REC.
           05 FILLER PIC X(17) VALUE SPACES.
           05 FILLER PIC X(45)
                VALUE "Polytechnic University of the Philippines".
           05 FILLER PIC X(18) VALUE SPACES.

       01  HD2-REC.
           05 FILLER PIC X(31) VALUE SPACES.
           05 FILLER PIC X(18) VALUE "Sta. Mesa, Manila".
           05 FILLER PIC X(31) VALUE SPACES.

       01  HD3-REC.
           05 FILLER PIC X(33) VALUE SPACES.
           05 FILLER PIC X(14) VALUE "Payroll Report".
           05 FILLER PIC X(33) VALUE SPACES.

       01  COLHD1.
           05 FILLER PIC X(8) VALUE "Employee".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "Employee".
           05 FILLER PIC X(10) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "Employee".
           05 FILLER PIC X(8) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "Gross Salary".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "Net Pay".
           05 FILLER PIC X(9) VALUE SPACES.

       01  COLHD2.
           05 FILLER                   PIC X(3) VALUE "No.".
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(4) VALUE "Name".
           05 FILLER                   PIC X(14) VALUE SPACES.
           05 FILLER                   PIC X(9) VALUE "Type Name".
           05 FILLER                   PIC X(40) VALUE SPACES.

       01  REC-OUT.
           05 EMPNO-OUT     PIC X(10).
           05 FILLER        PIC X(3) VALUE SPACES.
           05 EMPNAME-OUT   PIC X(20).
           05 FILLER        PIC X(3) VALUE SPACES.
           05 ETYPE-OUT     PIC X(10).
           05 FILLER        PIC X(3) VALUE SPACES.
           05 GROSS-SALARY-OUT  PIC Z,ZZZ,ZZ9.99.
           05 FILLER        PIC X(3) VALUE SPACES.
           05 NET-PAY-OUT       PIC Z,ZZ9.99.
           05 FILLER        PIC X(12) VALUE SPACES.

       01  D-NAME.
           05 FILLER     PIC X VALUE SPACE.
           05 DEPT-NAME  PIC X(17) VALUE "Department Name: ".
           05 DEPT-NAME-OUT PIC X(20).
           05 FILLER     PIC X(42) VALUE SPACES.

       01  TOTEREC.
           05 FILLER     PIC X VALUE SPACE.
           05 FILLER     PIC X(24) VALUE "Total no. of Employees: ".
           05 ECTR-OUT   PIC Z9.
           05 FILLER     PIC X(52) VALUE SPACES.

       01  TOTFREC.
           05 FILLER     PIC X VALUE SPACE.
           05 FILLER     PIC X(22) VALUE "Total no. of Faculty: ".
           05 FCTR-OUT   PIC Z9.
           05 FILLER     PIC X(54) VALUE SPACES.

       01  TOTNPREC.
           05 FILLER        PIC X VALUE SPACE.
           05 FILLER        PIC X(15) VALUE "Total Net Pay: ".
           05 FILLER        PIC X VALUE "P".
           05 TOTNPREC-OUT    PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER        PIC X(48) VALUE SPACES.
        
       01 GRAND-TOT-REC.
           05 FILLER PIC X VALUE SPACE.
           05 FILLER PIC X(37) 
              VALUE "Total Net Pay for all departments : ".
           05 FILLER PIC X VALUE "P".
           05 GRAND-TOT-OUT PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER PIC X(26) VALUE SPACES.

       01  TEMP-VARIABLES.       
            05 TDEPT              PIC X VALUE "0".
            05 EMP-CTR            PIC 9(4) VALUE 0.
            05 FAC-CTR            PIC 9(4) VALUE 0.
            05 DEPT-NETPAY        PIC 9(9)V99 VALUE 0.
            05 GRAND-NETPAY       PIC 9(11)V99 VALUE 0.
            05 EOFSW              PIC 9 VALUE 0.

       01 CALC-FIELDS.
            05 GROSS-SALARY       PIC 9(7)V99.
            05 DEDUCTION          PIC 9(7)V99.
            05 NET-PAY            PIC 9(7)V99.
            05 DED-RATE           PIC V99.
       
       01 NUMERIC-FIELDS.
            05 WS-HOURS           PIC 9(5)V99.
            05 WS-RATE            PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN THRU INIT-END.
           PERFORM READ-RTN THRU READ-END.
           PERFORM PROCESS-LOOP THRU PROCESS-LOOP-END.
           PERFORM FINAL-DEPT-BREAK THRU FINAL-DEPT-END.
           PERFORM FINISH-RTN THRU FINISH-END.
           STOP RUN.

       PROCESS-LOOP.
                PERFORM UNTIL EOFSW = 1
                PERFORM PROCESS-RTN THRU PROCESS-END
            END-PERFORM.
       PROCESS-LOOP-END.

       INIT-RTN.
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE.
           WRITE OUTREC FROM HD1-REC.
           WRITE OUTREC FROM HD2-REC.
           WRITE OUTREC FROM HD3-REC AFTER ADVANCING 2 LINES.
           MOVE 0 TO TDEPT.
       INIT-END.

       READ-RTN.
            READ INFILE
               AT END
                   MOVE 1 TO EOFSW
            END-READ.
       READ-END.

       PROCESS-RTN.
           IF EOFSW = 1
                GO TO PROCESS-END.

           IF DEPTC-IN NOT EQUAL TO TDEPT
               PERFORM DEPT-BREAK-RTN THRU DEPT-END.

           PERFORM CALC-PAY-RTN THRU CALC-END.

           IF ETYPE-IN = "Fa"
               MOVE "Faculty"   TO ETYPE-OUT
               ADD 1 TO FAC-CTR
           ELSE
               MOVE "Employee" TO ETYPE-OUT.

           MOVE EMPNO-IN                        TO EMPNO-OUT.
           MOVE EMPNAME-IN                      TO EMPNAME-OUT.
           MOVE GROSS-SALARY OF CALC-FIELDS     TO GROSS-SALARY-OUT.
           MOVE NET-PAY OF CALC-FIELDS          TO NET-PAY-OUT.
           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 1 LINES.

           ADD 1       TO EMP-CTR.
           ADD NET-PAY OF CALC-FIELDS TO DEPT-NETPAY.

           PERFORM READ-RTN THRU READ-END.
       PROCESS-END.

       CALC-PAY-RTN.
           MOVE FUNCTION NUMVAL(HOURS-STR) TO WS-HOURS.
           MOVE FUNCTION NUMVAL(RATE-STR) TO WS-RATE.
           COMPUTE GROSS-SALARY OF CALC-FIELDS = WS-HOURS * WS-RATE.

           IF GROSS-SALARY OF CALC-FIELDS <= 7000
               MOVE 0.10 TO DED-RATE
           ELSE IF GROSS-SALARY OF CALC-FIELDS <= 10000
               MOVE 0.15 TO DED-RATE
           ELSE IF GROSS-SALARY OF CALC-FIELDS <= 15000
               MOVE 0.20 TO DED-RATE
           ELSE
               MOVE 0.25 TO DED-RATE.

           COMPUTE DEDUCTION = GROSS-SALARY OF CALC-FIELDS * DED-RATE.
           COMPUTE NET-PAY OF CALC-FIELDS =
                GROSS-SALARY OF CALC-FIELDS - DEDUCTION.
       CALC-END.

       DEPT-BREAK-RTN.
           IF EMP-CTR > 0
               MOVE EMP-CTR TO ECTR-OUT
               WRITE OUTREC FROM TOTEREC AFTER ADVANCING 1 LINES
               MOVE FAC-CTR TO FCTR-OUT
               WRITE OUTREC FROM TOTFREC AFTER ADVANCING 1 LINES
               MOVE DEPT-NETPAY TO TOTNPREC-OUT
               WRITE OUTREC FROM TOTNPREC AFTER ADVANCING 1 LINES
               ADD DEPT-NETPAY TO GRAND-NETPAY
               MOVE 0 TO EMP-CTR
               MOVE 0 TO FAC-CTR
               MOVE 0 TO DEPT-NETPAY.

           IF DEPTC-IN = "1"
               MOVE "Administration Staff" TO DEPT-NAME-OUT
           ELSE IF DEPTC-IN = "2"
               MOVE "Academic Staff" TO DEPT-NAME-OUT
           END-IF.

           WRITE OUTREC FROM D-NAME AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM COLHD1 AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM COLHD2 AFTER ADVANCING 1 LINES.
           MOVE DEPTC-IN TO TDEPT.
       DEPT-END.

       FINAL-DEPT-BREAK.
            IF EMP-CTR > 0
                MOVE EMP-CTR TO ECTR-OUT
                WRITE OUTREC FROM TOTEREC AFTER ADVANCING 1 LINES
                MOVE FAC-CTR TO FCTR-OUT
                WRITE OUTREC FROM TOTFREC AFTER ADVANCING 1 LINES
                MOVE DEPT-NETPAY TO TOTNPREC-OUT
                WRITE OUTREC FROM TOTNPREC AFTER ADVANCING 1 LINES
                ADD DEPT-NETPAY TO GRAND-NETPAY.
       FINAL-DEPT-END.

       FINISH-RTN.
           MOVE GRAND-NETPAY TO GRAND-TOT-OUT.
           WRITE OUTREC FROM GRAND-TOT-REC AFTER ADVANCING 2 LINES.
           CLOSE INFILE OUTFILE.
       FINISH-END.
