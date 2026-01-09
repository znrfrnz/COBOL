       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.
       AUTHOR. STUDENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO "PAYROLL.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD              PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05  WS-CONTINUE             PIC X VALUE 'Y'.
               88  CONTINUE-YES        VALUE 'Y', 'y'.
               88  CONTINUE-NO         VALUE 'N', 'n'.
           05  WS-VALID-INPUT          PIC X VALUE 'N'.
               88  INPUT-IS-VALID      VALUE 'Y'.

       01  WS-INPUT-VARS.
           05  IN-EMP-NO               PIC X(5).
           05  IN-EMP-NAME             PIC X(25).
           05  IN-RATE-RAW             PIC X(10).
           05  IN-HOURS-RAW            PIC X(10).
           05  IN-GSIS-RAW             PIC X(10).
           05  IN-PHIL-RAW             PIC X(10).
           05  IN-PAGIBIG-RAW          PIC X(10).
           05  IN-TAX-RAW              PIC X(10).

       01  WS-CALC-VARS.
           05  WS-RATE                 PIC 9(3)V99.
           05  WS-HOURS                PIC 9(3).
           05  WS-GSIS                 PIC 9(4)V99.
           05  WS-PHILHEALTH           PIC 9(3)V99.
           05  WS-PAGIBIG              PIC 9(4)V99.
           05  WS-TAX                  PIC 9(4)V99.
           05  WS-GROSS-SALARY         PIC 9(5)V99.
           05  WS-TOTAL-DEDUCTIONS     PIC 9(5)V99.
           05  WS-NET-PAY              PIC 9(5)V99.

       01  WS-ACCUMULATORS.
           05  WS-TOTAL-EMP            PIC 9(5) VALUE 0.
           05  WS-TOTAL-SAL            PIC 9(7)V99 VALUE 0.

       01  WS-DISPLAY-FIELDS.
           05  DSP-GROSS               PIC ZZ,999.99.
           05  DSP-DEDUCTIONS          PIC ZZ,999.99.
           05  DSP-NET-PAY             PIC ZZ,999.99.
           05  DSP-TOT-EMP             PIC ZZ,ZZ9.
           05  DSP-TOT-SAL             PIC Z,ZZZ,999.99.

       01  RPT-HEADER-1.
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(40) VALUE 
               "POLYTECHNICUNIVERSITY OF THE PHILIPPINES".
           05  FILLER                  PIC X(20) VALUE SPACES.

       01  RPT-HEADER-2.
           05  FILLER                  PIC X(31) VALUE SPACES.
           05  FILLER                  PIC X(14) VALUE "PAYROLL REPORT".
           05  FILLER                  PIC X(35) VALUE SPACES.

       01  RPT-HEADER-3.
           05  FILLER                  PIC X(6)  VALUE SPACES.
           05  FILLER                  PIC X(8)  VALUE "EMPLOYEE".
           05  FILLER                  PIC X(18) VALUE SPACES.
           05  FILLER                  PIC X(8)  VALUE "EMPLOYEE".
           05  FILLER                  PIC X(40) VALUE SPACES.

       01  RPT-HEADER-4.
           05  FILLER                  PIC X(7)  VALUE SPACES.
           05  FILLER                  PIC X(6)  VALUE "NUMBER".
           05  FILLER                  PIC X(21) VALUE SPACES.
           05  FILLER                  PIC X(4)  VALUE "NAME".
           05  FILLER                  PIC X(21) VALUE SPACES.
           05  FILLER                  PIC X(3)  VALUE "NET".
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  RPT-HEADER-5.
           05  FILLER                  PIC X(59) VALUE SPACES.
           05  FILLER                  PIC X(3)  VALUE "PAY".
           05  FILLER                  PIC X(18) VALUE SPACES.

       01  RPT-DETAIL.
           05  FILLER                  PIC X(7)  VALUE SPACES.
           05  RPT-EMP-NO              PIC X(5).
           05  FILLER                  PIC X(18) VALUE SPACES.
           05  RPT-EMP-NAME            PIC X(25).
           05  FILLER                  PIC X(4)  VALUE SPACES.
           05  RPT-NET-PAY             PIC ZZ,999.99.
           05  FILLER                  PIC X(12) VALUE SPACES.

       01  RPT-FOOTER-1.
           05  FILLER                  PIC X(27) VALUE 
               "TOTAL NUMBER OF EMPLOYEES: ".
           05  RPT-TOT-EMP             PIC ZZ,ZZ9.
           05  FILLER                  PIC X(46) VALUE SPACES.

       01  RPT-FOOTER-2.
           05  FILLER                  PIC X(28) VALUE 
               "TOTAL ACCUMULATED SALARIES: ".
           05  RPT-TOT-SAL             PIC Z,ZZZ,999.99.
           05  FILLER                  PIC X(39) VALUE SPACES.

       SCREEN SECTION.
       01  MAIN-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COL 5 VALUE "ENTER EMPLOYEE NO.:".
           05  LINE 3 COL 5 VALUE "ENTER EMPLOYEE NAME:".
           05  LINE 6 COL 5 VALUE "GROSS SALARY:".
           05  LINE 12 COL 5 VALUE "DEDUCTIONS:".
           05  LINE 14 COL 5 VALUE "NET PAY:".
           05  LINE 16 COL 5 VALUE "ENTER ANOTHER [Y/N]:".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT PAYROLL-FILE
           PERFORM WRITE-HEADERS

           PERFORM UNTIL CONTINUE-NO
               DISPLAY MAIN-SCREEN
               
               ACCEPT IN-EMP-NO AT LINE 2 COLUMN 26
               ACCEPT IN-EMP-NAME AT LINE 3 COLUMN 26
               
               PERFORM GET-RATE
               PERFORM GET-HOURS
               
               COMPUTE WS-GROSS-SALARY = WS-RATE * WS-HOURS
               MOVE WS-GROSS-SALARY TO DSP-GROSS
               DISPLAY DSP-GROSS AT LINE 6 COLUMN 26

               PERFORM GET-GSIS
               PERFORM GET-PHILHEALTH
               PERFORM GET-PAGIBIG
               PERFORM GET-TAX

               COMPUTE WS-TOTAL-DEDUCTIONS = WS-GSIS + WS-PHILHEALTH 
                       + WS-PAGIBIG + WS-TAX
               MOVE WS-TOTAL-DEDUCTIONS TO DSP-DEDUCTIONS
               DISPLAY DSP-DEDUCTIONS AT LINE 12 COLUMN 26

               COMPUTE WS-NET-PAY = WS-GROSS-SALARY - 
                                    WS-TOTAL-DEDUCTIONS
               MOVE WS-NET-PAY TO DSP-NET-PAY
               DISPLAY DSP-NET-PAY AT LINE 14 COLUMN 26

               ADD 1 TO WS-TOTAL-EMP
               ADD WS-NET-PAY TO WS-TOTAL-SAL

               PERFORM WRITE-DETAIL

               MOVE 'N' TO WS-VALID-INPUT
               PERFORM UNTIL INPUT-IS-VALID
                   DISPLAY "ENTER ANOTHER [Y/N]:" AT LINE 16 COLUMN 5
                   ACCEPT WS-CONTINUE AT LINE 16 COLUMN 27
                   IF CONTINUE-YES OR CONTINUE-NO
                       MOVE 'Y' TO WS-VALID-INPUT
                   ELSE
                       DISPLAY "INVALID INPUT " AT LINE 16 COLUMN 35
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM WRITE-FOOTERS
           PERFORM DISPLAY-TOTALS
           
           CLOSE PAYROLL-FILE
           STOP RUN.

       GET-RATE.
           MOVE 'N' TO WS-VALID-INPUT
           PERFORM UNTIL INPUT-IS-VALID
               DISPLAY "RATE PER HOUR:" AT LINE 4 COLUMN 5
               ACCEPT IN-RATE-RAW AT LINE 4 COLUMN 26
               IF FUNCTION TEST-NUMVAL(IN-RATE-RAW) = 0
                   COMPUTE WS-RATE = FUNCTION NUMVAL(IN-RATE-RAW)
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "INVALID INPUT " AT LINE 4 COLUMN 40
               END-IF
           END-PERFORM.

       GET-HOURS.
           MOVE 'N' TO WS-VALID-INPUT
           PERFORM UNTIL INPUT-IS-VALID
               DISPLAY "NO. OF HOURS WORKED:" AT LINE 5 COLUMN 5
               ACCEPT IN-HOURS-RAW AT LINE 5 COLUMN 26
               IF FUNCTION TEST-NUMVAL(IN-HOURS-RAW) = 0
                   COMPUTE WS-HOURS = FUNCTION NUMVAL(IN-HOURS-RAW)
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "INVALID INPUT " AT LINE 5 COLUMN 40
               END-IF
           END-PERFORM.

       GET-GSIS.
           MOVE 'N' TO WS-VALID-INPUT
           PERFORM UNTIL INPUT-IS-VALID
               DISPLAY "GSIS CONTRIBUTION:" AT LINE 8 COLUMN 5
               ACCEPT IN-GSIS-RAW AT LINE 8 COLUMN 26
               IF FUNCTION TEST-NUMVAL(IN-GSIS-RAW) = 0
                   COMPUTE WS-GSIS = FUNCTION NUMVAL(IN-GSIS-RAW)
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "INVALID INPUT " AT LINE 8 COLUMN 40
               END-IF
           END-PERFORM.

       GET-PHILHEALTH.
           MOVE 'N' TO WS-VALID-INPUT
           PERFORM UNTIL INPUT-IS-VALID
               DISPLAY "PHILHEALTH:" AT LINE 9 COLUMN 5
               ACCEPT IN-PHIL-RAW AT LINE 9 COLUMN 26
               IF FUNCTION TEST-NUMVAL(IN-PHIL-RAW) = 0
                   COMPUTE WS-PHILHEALTH = FUNCTION NUMVAL(IN-PHIL-RAW)
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "INVALID INPUT " AT LINE 9 COLUMN 40
               END-IF
           END-PERFORM.

       GET-PAGIBIG.
           MOVE 'N' TO WS-VALID-INPUT
           PERFORM UNTIL INPUT-IS-VALID
               DISPLAY "PAG-IBIG:" AT LINE 10 COLUMN 5
               ACCEPT IN-PAGIBIG-RAW AT LINE 10 COLUMN 26
               IF FUNCTION TEST-NUMVAL(IN-PAGIBIG-RAW) = 0
                   COMPUTE WS-PAGIBIG = FUNCTION NUMVAL(IN-PAGIBIG-RAW)
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "INVALID INPUT " AT LINE 10 COLUMN 40
               END-IF
           END-PERFORM.

       GET-TAX.
           MOVE 'N' TO WS-VALID-INPUT
           PERFORM UNTIL INPUT-IS-VALID
               DISPLAY "WITHHOLDING TAX:" AT LINE 11 COLUMN 5
               ACCEPT IN-TAX-RAW AT LINE 11 COLUMN 26
               IF FUNCTION TEST-NUMVAL(IN-TAX-RAW) = 0
                   COMPUTE WS-TAX = FUNCTION NUMVAL(IN-TAX-RAW)
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "INVALID INPUT " AT LINE 11 COLUMN 40
               END-IF
           END-PERFORM.

       WRITE-HEADERS.
           WRITE PAYROLL-RECORD FROM RPT-HEADER-1
           WRITE PAYROLL-RECORD FROM RPT-HEADER-2
           WRITE PAYROLL-RECORD FROM RPT-HEADER-3
           WRITE PAYROLL-RECORD FROM RPT-HEADER-4
           WRITE PAYROLL-RECORD FROM RPT-HEADER-5.

       WRITE-DETAIL.
           MOVE IN-EMP-NO TO RPT-EMP-NO
           MOVE IN-EMP-NAME TO RPT-EMP-NAME
           MOVE WS-NET-PAY TO RPT-NET-PAY
           WRITE PAYROLL-RECORD FROM RPT-DETAIL.

       WRITE-FOOTERS.
           MOVE WS-TOTAL-EMP TO RPT-TOT-EMP
           MOVE WS-TOTAL-SAL TO RPT-TOT-SAL
           WRITE PAYROLL-RECORD FROM RPT-FOOTER-1 AFTER 2 LINES
           WRITE PAYROLL-RECORD FROM RPT-FOOTER-2.

       DISPLAY-TOTALS.
           MOVE WS-TOTAL-EMP TO DSP-TOT-EMP
           MOVE WS-TOTAL-SAL TO DSP-TOT-SAL
           DISPLAY " " AT LINE 1 COLUMN 1 WITH BLANK SCREEN
           DISPLAY "TOTAL NUMBER OF EMPLOYEES: " AT LINE 5 COLUMN 5
           DISPLAY DSP-TOT-EMP AT LINE 5 COLUMN 35
           DISPLAY "TOTAL ACCUMULATED SALARIES: " AT LINE 6 COLUMN 5
           DISPLAY DSP-TOT-SAL AT LINE 6 COLUMN 35
           DISPLAY "end of program, please press a key to exit" 
               AT LINE 8 COLUMN 5
           ACCEPT WS-CONTINUE AT LINE 8 COLUMN 48.

