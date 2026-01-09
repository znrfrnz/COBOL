       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACT3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'infile.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO 'outfile.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-REC.
           05 IN-ACCNO        PIC X(10).
           05 IN-ACCNAME      PIC X(25).
           05 IN-TRANSCODE    PIC X(1).
           05 IN-AMOUNT       PIC 9(7)V99.

       FD  OUTFILE.
       01  OUT-REC            PIC X(80).

       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05 EOF-SWITCH      PIC X VALUE 'N'.
              88 END-OF-FILE  VALUE 'Y'.

       01  COUNTERS.
           05 TOT-RECORDS     PIC 9(5) VALUE 0.
           05 TOT-ACC-BAL     PIC S9(10)V99 VALUE 0.

       01  HOLDING-FIELDS.
           05 PREV-ACCNO      PIC X(10) VALUE SPACES.
           05 PREV-ACCNAME    PIC X(25) VALUE SPACES.
           05 SUB-BALANCE     PIC S9(7)V99 VALUE 0.

       01  HEADER-1.
           05 FILLER          PIC X(25) VALUE SPACES.
           05 FILLER          PIC X(16) VALUE 'China Trust Bank'.
           05 FILLER          PIC X(39) VALUE SPACES.

       01  HEADER-2.
           05 FILLER          PIC X(27) VALUE SPACES.
           05 FILLER          PIC X(13) VALUE 'Makati Avenue'.
           05 FILLER          PIC X(40) VALUE SPACES.

       01  HEADER-3.
           05 FILLER          PIC X(28) VALUE SPACES.
           05 FILLER          PIC X(11) VALUE 'Makati City'.
           05 FILLER          PIC X(41) VALUE SPACES.

       01  HEADER-4.
           05 FILLER          PIC X(26) VALUE SPACES.
           05 FILLER          PIC X(16) VALUE 'Account''s Report'.
           05 FILLER          PIC X(38) VALUE SPACES.

       01  HEADER-5.
           05 FILLER          PIC X(5)  VALUE SPACES.
           05 FILLER          PIC X(7)  VALUE 'Account'.
           05 FILLER          PIC X(18) VALUE SPACES.
           05 FILLER          PIC X(4)  VALUE 'Name'.
           05 FILLER          PIC X(21) VALUE SPACES.
           05 FILLER          PIC X(7)  VALUE 'Balance'.

       01  HEADER-6.
           05 FILLER          PIC X(5)  VALUE SPACES.
           05 FILLER          PIC X(6)  VALUE 'Number'.
           05 FILLER          PIC X(69) VALUE SPACES.

       01  DETAIL-LINE.
           05 FILLER          PIC X(5) VALUE SPACES.
           05 DET-ACCNO       PIC X(10).
           05 FILLER          PIC X(5) VALUE SPACES.
           05 DET-ACCNAME     PIC X(25).
           05 FILLER          PIC X(5) VALUE SPACES.
           05 DET-BALANCE     PIC ZZZ,ZZZ,ZZ9.99.

       01  TOTAL-REC-LINE.
           05 FILLER          PIC X(5) VALUE SPACES.
           05 FILLER          PIC X(29) VALUE
              'Total No. of Records printed:'.
           05 FILLER          PIC X(2) VALUE SPACES.
           05 OUT-TOT-REC     PIC Z(4)9.

       01  TOTAL-BAL-LINE.
           05 FILLER          PIC X(5) VALUE SPACES.
           05 FILLER          PIC X(26) VALUE
              'Total Accumulated Balance:'.
           05 FILLER          PIC X(2) VALUE ' P'.
           05 OUT-TOT-BAL     PIC Z,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INFILE
                OUTPUT OUTFILE.

           PERFORM WRITE-HEADERS.

           READ INFILE
               AT END MOVE 'Y' TO EOF-SWITCH
           END-READ.

           IF NOT END-OF-FILE
               MOVE IN-ACCNO TO PREV-ACCNO
               MOVE IN-ACCNAME TO PREV-ACCNAME
           END-IF.

           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.

      * Process the last group
           IF PREV-ACCNO NOT = SPACES
               PERFORM WRITE-GROUP-SUMMARY
           END-IF.

           PERFORM WRITE-FINAL-TOTALS.

           CLOSE INFILE
                 OUTFILE.
           STOP RUN.

       WRITE-HEADERS.
           WRITE OUT-REC FROM HEADER-1.
           WRITE OUT-REC FROM HEADER-2.
           WRITE OUT-REC FROM HEADER-3.
           WRITE OUT-REC FROM SPACES.
           WRITE OUT-REC FROM SPACES.
           WRITE OUT-REC FROM HEADER-4.
           WRITE OUT-REC FROM SPACES.
           WRITE OUT-REC FROM HEADER-5.
           WRITE OUT-REC FROM HEADER-6.
           WRITE OUT-REC FROM SPACES.

       PROCESS-RECORDS.
           IF IN-ACCNO NOT = PREV-ACCNO
               PERFORM WRITE-GROUP-SUMMARY
               MOVE IN-ACCNO TO PREV-ACCNO
               MOVE IN-ACCNAME TO PREV-ACCNAME
               MOVE 0 TO SUB-BALANCE
           END-IF.

           IF IN-TRANSCODE = 'D'
               ADD IN-AMOUNT TO SUB-BALANCE
           ELSE IF IN-TRANSCODE = 'W'
               SUBTRACT IN-AMOUNT FROM SUB-BALANCE
           END-IF.

           READ INFILE
               AT END MOVE 'Y' TO EOF-SWITCH
           END-READ.

       WRITE-GROUP-SUMMARY.
           MOVE PREV-ACCNO TO DET-ACCNO.
           MOVE PREV-ACCNAME TO DET-ACCNAME.
           MOVE SUB-BALANCE TO DET-BALANCE.
           WRITE OUT-REC FROM DETAIL-LINE.

           ADD 1 TO TOT-RECORDS.
           ADD SUB-BALANCE TO TOT-ACC-BAL.

       WRITE-FINAL-TOTALS.
           WRITE OUT-REC FROM SPACES.
           MOVE TOT-RECORDS TO OUT-TOT-REC.
           WRITE OUT-REC FROM TOTAL-REC-LINE.
           MOVE TOT-ACC-BAL TO OUT-TOT-BAL.
           WRITE OUT-REC FROM TOTAL-BAL-LINE.
