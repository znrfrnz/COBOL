       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.
       AUTHOR. FRANZ-VINCENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE ASSIGN TO "PAYROLL.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE.
       01  OUT-REC                    PIC X(80).

       WORKING-STORAGE SECTION.
       
      * INPUT VARIABLES
       01  EMP-NO                     PIC X(5).
       01  EMP-NAME                   PIC X(25).
       01  RATE-HOUR                  PIC 9(3)V99.
       01  HOURS                      PIC 9(3).
       01  GSIS                       PIC 9(4)V99.
       01  PHILHEALTH                 PIC 9(3)V99.
       01  PAGIBIG                    PIC 9(4)V99.
       01  TAX                        PIC 9(4)V99.

      * INPUT BUFFER VARIABLES
       01  IN-RATE                    PIC X(10).
       01  IN-HOURS                   PIC X(10).
       01  IN-GSIS                    PIC X(10).
       01  IN-PHIL                    PIC X(10).
       01  IN-PAGIBIG                 PIC X(10).
       01  IN-TAX                     PIC X(10).

      * COMPUTED VARIABLES
       01  GROSS-PAY                  PIC 9(5)V99.
       01  TOT-DED                    PIC 9(5)V99.
       01  NET-PAY                    PIC 9(5)V99.

      * DISPLAY VARIABLES (EDITED PICTURE STRINGS)
       01  D-GROSS                    PIC ZZ,999.99.
       01  D-DED                      PIC ZZ,999.99.
       01  D-NET                      PIC ZZ,999.99.
       01  D-TOT-EMP                  PIC ZZ,ZZ9.
       01  D-TOT-SAL                  PIC Z,ZZZ,999.99.

      * ACCUMULATORS
       01  CTR-EMP                    PIC 9(5) VALUE 0.
       01  ACC-SAL                    PIC 9(7)V99 VALUE 0.

      * CONTROL FLAGS
       01  ANOTHER                    PIC X VALUE 'Y'.

      * REPORT LINES
       01  HDR-1.
           05 FILLER                  PIC X(20) VALUE SPACES.
           05 FILLER                  PIC X(45) VALUE 
              "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES".
           05 FILLER                  PIC X(15) VALUE SPACES.

       01  HDR-2.
           05 FILLER                  PIC X(32) VALUE SPACES.
           05 FILLER                  PIC X(20) VALUE "PAYROLL REPORT".
           05 FILLER                  PIC X(28) VALUE SPACES.

       01  COL-HDR-1.
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "EMPLOYEE".
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "EMPLOYEE".
           05 FILLER                  PIC X(20) VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "   NET    ".

       01  COL-HDR-2.
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE " NUMBER ".
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "  NAME  ".
           05 FILLER                  PIC X(20) VALUE SPACES.
           05 FILLER                  PIC X(10) VALUE "   PAY    ".

       01  DET-LINE.
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 R-EMP-NO                PIC X(5).
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 R-EMP-NAME              PIC X(25).
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 R-NET-PAY               PIC ZZ,999.99.
           05 FILLER                  PIC X(15) VALUE SPACES.

       01  FTR-1.
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 FILLER                  PIC X(27) VALUE 
              "TOTAL NUMBER OF EMPLOYEES: ".
           05 R-TOT-EMP               PIC ZZ,ZZ9.
           05 FILLER                  PIC X(40) VALUE SPACES.

       01  FTR-2.
           05 FILLER                  PIC X(5)  VALUE SPACES.
           05 FILLER                  PIC X(28) VALUE 
              "TOTAL ACCUMULATED SALARIES: ".
           05 R-TOT-SAL               PIC Z,ZZZ,999.99.
           05 FILLER                  PIC X(30) VALUE SPACES.
       
       01  BLANK-LINE                 PIC X(80) VALUE SPACES.

       SCREEN SECTION.
       01  CLR-SCREEN.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT OUT-FILE.
           PERFORM WRITE-HEADERS.
           
           PERFORM PROCESS-EMPLOYEE UNTIL ANOTHER = 'N' OR 'n'.
           
           PERFORM WRITE-FOOTERS.
           PERFORM DISPLAY-FINAL-TOTALS.
           
           CLOSE OUT-FILE.
           STOP RUN.

       WRITE-HEADERS.
           WRITE OUT-REC FROM HDR-1.
           WRITE OUT-REC FROM BLANK-LINE.
           WRITE OUT-REC FROM HDR-2.
           WRITE OUT-REC FROM BLANK-LINE.
           WRITE OUT-REC FROM COL-HDR-1.
           WRITE OUT-REC FROM COL-HDR-2.
           WRITE OUT-REC FROM BLANK-LINE.

       PROCESS-EMPLOYEE.
           INITIALIZE IN-RATE IN-HOURS
                      IN-GSIS IN-PHIL
                      IN-PAGIBIG IN-TAX.
           DISPLAY CLR-SCREEN.
           DISPLAY "ENTER EMPLOYEE NO.: " AT 0310.
           ACCEPT EMP-NO AT 0335.
           
           DISPLAY "ENTER EMPLOYEE NAME:" AT 0410.
           ACCEPT EMP-NAME AT 0435.
           
           DISPLAY "RATE PER HOUR:      " AT 0510.
           ACCEPT IN-RATE AT 0535.
           COMPUTE RATE-HOUR = FUNCTION NUMVAL(IN-RATE).
           
           DISPLAY "NO. OF HOURS WORKED:" AT 0610.
           ACCEPT IN-HOURS AT 0635.
           COMPUTE HOURS = FUNCTION NUMVAL(IN-HOURS).
           
           COMPUTE GROSS-PAY = RATE-HOUR * HOURS.
           MOVE GROSS-PAY TO D-GROSS.
           DISPLAY "GROSS SALARY:       " AT 0710
                   D-GROSS AT 0735.
           
           DISPLAY "GSIS CONTRIBUTION:  " AT 0910.
           ACCEPT IN-GSIS AT 0935.
           COMPUTE GSIS = FUNCTION NUMVAL(IN-GSIS).
           
           DISPLAY "PHILHEALTH:         " AT 1010.
           ACCEPT IN-PHIL AT 1035.
           COMPUTE PHILHEALTH = FUNCTION NUMVAL(IN-PHIL).
           
           DISPLAY "PAG-IBIG:           " AT 1110.
           ACCEPT IN-PAGIBIG AT 1135.
           COMPUTE PAGIBIG = FUNCTION NUMVAL(IN-PAGIBIG).
           
           DISPLAY "WITHOLDING TAX:     " AT 1210.
           ACCEPT IN-TAX AT 1235.
           COMPUTE TAX = FUNCTION NUMVAL(IN-TAX).
           
           COMPUTE TOT-DED = GSIS + 
                   PHILHEALTH + PAGIBIG + TAX.
           MOVE TOT-DED TO D-DED.
           DISPLAY "DEDUCTIONS:         " AT 1310
                   D-DED AT 1335.
           
           COMPUTE NET-PAY = GROSS-PAY - TOT-DED.
           MOVE NET-PAY TO D-NET.
           DISPLAY "NET PAY:            " AT 1510
                   D-NET AT 1535.
           
           ADD 1 TO CTR-EMP.
           ADD NET-PAY TO ACC-SAL.
           
           PERFORM WRITE-DETAIL.
           
           MOVE SPACE TO ANOTHER.
           PERFORM UNTIL ANOTHER = 'Y' OR 'y' OR 'N' OR 'n'
               DISPLAY "ENTER ANOTHER [Y/N]: " AT 1710
               ACCEPT ANOTHER AT 1735
               IF ANOTHER NOT = 'Y' AND ANOTHER NOT = 'y' AND
                  ANOTHER NOT = 'N' AND ANOTHER NOT = 'n'
                   DISPLAY "INVALID INPUT! (Y/N)" AT 1740
               ELSE
                   DISPLAY "                      " AT 1740
               END-IF
           END-PERFORM.

       WRITE-DETAIL.
           MOVE EMP-NO TO R-EMP-NO.
           MOVE EMP-NAME TO R-EMP-NAME.
           MOVE NET-PAY TO R-NET-PAY.
           WRITE OUT-REC FROM DET-LINE.

       WRITE-FOOTERS.
           WRITE OUT-REC FROM BLANK-LINE.
           MOVE CTR-EMP TO R-TOT-EMP.
           WRITE OUT-REC FROM FTR-1.
           MOVE ACC-SAL TO R-TOT-SAL.
           WRITE OUT-REC FROM FTR-2.

       DISPLAY-FINAL-TOTALS.
           DISPLAY CLR-SCREEN.
           MOVE CTR-EMP TO D-TOT-EMP.
           MOVE ACC-SAL TO D-TOT-SAL.
           
           DISPLAY "TOTAL NUMBER OF EMPLOYEES: " AT 1010
                   D-TOT-EMP.
           DISPLAY "TOTAL ACCUMULATED SALARIES:" AT 1110
                   D-TOT-SAL.
