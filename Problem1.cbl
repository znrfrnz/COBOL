       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-REPORT.
       AUTHOR. COBOL-PROGRAMMER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "PAYROLL.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05 DEPT-CODE                PIC 9.
           05 EMP-NUMBER               PIC X(10).
           05 EMP-NAME                 PIC X(25).
           05 EMP-TYPE-CODE            PIC X(2).
           05 HOURS-WORKED             PIC 9(5)V99.
           05 RATE-PER-HOUR            PIC 9(5)V99.
       
       FD  REPORT-FILE.
       01  REPORT-LINE                 PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF                      PIC X VALUE 'N'.
       01  WS-CURRENT-DEPT             PIC 9 VALUE 0.
       01  WS-GROSS-SALARY             PIC 9(7)V99.
       01  WS-DEDUCTION-RATE           PIC 9V99.
       01  WS-DEDUCTION                PIC 9(7)V99.
       01  WS-NET-PAY                  PIC 9(7)V99.
       
       01  WS-DEPT-EMP-COUNT           PIC 9(4) VALUE 0.
       01  WS-DEPT-FACULTY-COUNT       PIC 9(4) VALUE 0.
       01  WS-DEPT-NET-PAY             PIC 9(9)V99 VALUE 0.
       01  WS-TOTAL-NET-PAY            PIC 9(11)V99 VALUE 0.
       
       01  WS-DEPT-NAME                PIC X(20).
       
       01  HEADER-LINE-1.
           05 FILLER                   PIC X(17) VALUE SPACES.
           05 FILLER                   PIC X(45) 
              VALUE "Polytechnic University of the Philippines".
           05 FILLER                   PIC X(18) VALUE SPACES.
       
       01  HEADER-LINE-2.
           05 FILLER                   PIC X(31) VALUE SPACES.
           05 FILLER                   PIC X(18) 
              VALUE "Sta. Mesa, Manila".
           05 FILLER                   PIC X(31) VALUE SPACES.
       
       01  HEADER-LINE-3.
           05 FILLER                   PIC X(80) VALUE SPACES.
       
       01  HEADER-LINE-4.
           05 FILLER                   PIC X(33) VALUE SPACES.
           05 FILLER                   PIC X(14) VALUE "Payroll Report".
           05 FILLER                   PIC X(33) VALUE SPACES.
       
       01  HEADER-LINE-5.
           05 FILLER                   PIC X(80) VALUE SPACES.
       
       01  COLUMN-HEADER-1.
           05 FILLER                   PIC X(8) VALUE "Employee".
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 FILLER                   PIC X(8) VALUE "Employee".
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(8) VALUE "Employee".
           05 FILLER                   PIC X(8) VALUE SPACES.
           05 FILLER                   PIC X(12) VALUE "Gross Salary".
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 FILLER                   PIC X(7) VALUE "Net Pay".
           05 FILLER                   PIC X(9) VALUE SPACES.
       
       01  COLUMN-HEADER-2.
           05 FILLER                   PIC X(3) VALUE "No.".
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(4) VALUE "Name".
           05 FILLER                   PIC X(14) VALUE SPACES.
           05 FILLER                   PIC X(9) VALUE "Type Name".
           05 FILLER                   PIC X(40) VALUE SPACES.
       
       01  COLUMN-HEADER-3.
           05 FILLER                   PIC X(5) VALUE "X(10)".
           05 FILLER                   PIC X(8) VALUE SPACES.
           05 FILLER                   PIC X(5) VALUE "X(25)".
           05 FILLER                   PIC X(13) VALUE SPACES.
           05 FILLER                   PIC X(5) VALUE "X(10)".
           05 FILLER                   PIC X(8) VALUE SPACES.
           05 FILLER                   PIC X(11) VALUE "P999,999.99".
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 FILLER                   PIC X(10) VALUE "P99,999.99".
           05 FILLER                   PIC X(12) VALUE SPACES.
       
       01  DETAIL-LINE.
           05 DL-EMP-NUMBER            PIC X(10).
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 DL-EMP-NAME              PIC X(20).
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 DL-EMP-TYPE-NAME         PIC X(10).
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 DL-GROSS-SALARY          PIC Z,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 DL-NET-PAY               PIC Z,ZZ9.99.
           05 FILLER                   PIC X(12) VALUE SPACES.
       
       01  DEPT-BREAK-LINE-1.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(17) 
              VALUE "Department Name: ".
           05 DBL-DEPT-NAME            PIC X(20).
           05 FILLER                   PIC X(42) VALUE SPACES.
       
       01  DEPT-BREAK-LINE-2.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(24) 
              VALUE "Total no. of Employees: ".
           05 DBL-EMP-COUNT            PIC Z9.
           05 FILLER                   PIC X(52) VALUE SPACES.
       
       01  DEPT-BREAK-LINE-3.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(22) 
              VALUE "Total no. of Faculty: ".
           05 DBL-FACULTY-COUNT        PIC Z9.
           05 FILLER                   PIC X(54) VALUE SPACES.
       
       01  DEPT-BREAK-LINE-4.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(15) 
              VALUE "Total Net Pay: ".
           05 FILLER                   PIC X VALUE "P".
           05 DBL-DEPT-NET-PAY         PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(48) VALUE SPACES.
       
       01  FINAL-TOTAL-LINE.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(37) 
              VALUE "Total Net Pay for all departments : ".
           05 FILLER                   PIC X VALUE "P".
           05 FTL-TOTAL-NET-PAY        PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(26) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT REPORT-FILE
           
           PERFORM WRITE-REPORT-HEADER
           
           PERFORM READ-EMPLOYEE
           
           PERFORM UNTIL WS-EOF = 'Y'
               IF DEPT-CODE NOT = SPACE AND DEPT-CODE NOT = ZERO
                   IF DEPT-CODE NOT = WS-CURRENT-DEPT
                       IF WS-CURRENT-DEPT NOT = 0
                           PERFORM WRITE-DEPT-BREAK
                       END-IF
                       MOVE DEPT-CODE TO WS-CURRENT-DEPT
                       PERFORM WRITE-DEPT-HEADER
                       PERFORM WRITE-COLUMN-HEADERS
                   END-IF
                   
                   PERFORM CALCULATE-PAY
                   PERFORM WRITE-DETAIL-LINE
                   PERFORM ACCUMULATE-TOTALS
               END-IF
               PERFORM READ-EMPLOYEE
           END-PERFORM
           
           PERFORM WRITE-DEPT-BREAK
           PERFORM WRITE-FINAL-TOTAL
           
           CLOSE EMPLOYEE-FILE
           CLOSE REPORT-FILE
           STOP RUN.
       
       READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ.
       
       CALCULATE-PAY.
           COMPUTE WS-GROSS-SALARY = HOURS-WORKED * RATE-PER-HOUR
           
           EVALUATE TRUE
               WHEN WS-GROSS-SALARY <= 7000
                   MOVE 0.10 TO WS-DEDUCTION-RATE
               WHEN WS-GROSS-SALARY <= 10000
                   MOVE 0.15 TO WS-DEDUCTION-RATE
               WHEN WS-GROSS-SALARY <= 15000
                   MOVE 0.20 TO WS-DEDUCTION-RATE
               WHEN WS-GROSS-SALARY > 15000
                   MOVE 0.25 TO WS-DEDUCTION-RATE
           END-EVALUATE
           
           COMPUTE WS-DEDUCTION = WS-GROSS-SALARY * WS-DEDUCTION-RATE
           COMPUTE WS-NET-PAY = WS-GROSS-SALARY - WS-DEDUCTION.
       
       ACCUMULATE-TOTALS.
           ADD 1 TO WS-DEPT-EMP-COUNT
           IF EMP-TYPE-CODE = "Fa"
               ADD 1 TO WS-DEPT-FACULTY-COUNT
           END-IF
           ADD WS-NET-PAY TO WS-DEPT-NET-PAY
           ADD WS-NET-PAY TO WS-TOTAL-NET-PAY.
       
       WRITE-REPORT-HEADER.
           WRITE REPORT-LINE FROM HEADER-LINE-1 AFTER ADVANCING PAGE
           WRITE REPORT-LINE FROM HEADER-LINE-2 AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HEADER-LINE-3 AFTER ADVANCING 2 LINES
           WRITE REPORT-LINE FROM HEADER-LINE-4 AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HEADER-LINE-5 AFTER ADVANCING 1 LINE.
       
       WRITE-DEPT-HEADER.
           EVALUATE DEPT-CODE
               WHEN 1
                   MOVE "Administration Staff" TO WS-DEPT-NAME
               WHEN 2
                   MOVE "Academic Staff" TO WS-DEPT-NAME
               WHEN OTHER
                   MOVE "Unknown Department" TO WS-DEPT-NAME
           END-EVALUATE.
       
       WRITE-COLUMN-HEADERS.
           WRITE REPORT-LINE FROM COLUMN-HEADER-1 
               AFTER ADVANCING 2 LINES
           WRITE REPORT-LINE FROM COLUMN-HEADER-2 
               AFTER ADVANCING 1 LINE.
       
       WRITE-DETAIL-LINE.
           MOVE EMP-NUMBER TO DL-EMP-NUMBER
           MOVE EMP-NAME TO DL-EMP-NAME
           
           EVALUATE EMP-TYPE-CODE
               WHEN "Fa"
                   MOVE "Faculty" TO DL-EMP-TYPE-NAME
               WHEN "Em"
                   MOVE "Employee" TO DL-EMP-TYPE-NAME
               WHEN OTHER
                   MOVE "Unknown" TO DL-EMP-TYPE-NAME
           END-EVALUATE
           
           MOVE WS-GROSS-SALARY TO DL-GROSS-SALARY
           MOVE WS-NET-PAY TO DL-NET-PAY
           
           WRITE REPORT-LINE FROM DETAIL-LINE 
               AFTER ADVANCING 1 LINE.
       
       WRITE-DEPT-BREAK.
           MOVE WS-DEPT-NAME TO DBL-DEPT-NAME
           MOVE WS-DEPT-EMP-COUNT TO DBL-EMP-COUNT
           MOVE WS-DEPT-FACULTY-COUNT TO DBL-FACULTY-COUNT
           MOVE WS-DEPT-NET-PAY TO DBL-DEPT-NET-PAY
           
           WRITE REPORT-LINE FROM DEPT-BREAK-LINE-1 
               AFTER ADVANCING 2 LINES
           WRITE REPORT-LINE FROM DEPT-BREAK-LINE-2 
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM DEPT-BREAK-LINE-3 
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM DEPT-BREAK-LINE-4 
               AFTER ADVANCING 1 LINE
           
           MOVE 0 TO WS-DEPT-EMP-COUNT
           MOVE 0 TO WS-DEPT-FACULTY-COUNT
           MOVE 0 TO WS-DEPT-NET-PAY.
       
       WRITE-FINAL-TOTAL.
           MOVE WS-TOTAL-NET-PAY TO FTL-TOTAL-NET-PAY
           WRITE REPORT-LINE FROM FINAL-TOTAL-LINE 
               AFTER ADVANCING 2 LINES.
