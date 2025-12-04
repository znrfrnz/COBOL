       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-TRANSACTION.
       AUTHOR. BAES-CHACON-CRISTOBAL-GOMEZ-INCIONG-JAVIER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "INFILE.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "OUTFILE.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  REC-IN. 
           05 ACCNO-IN             PIC X(3).
           05 FILLER               PIC X(1).
           05 ACCNAME-IN           PIC X(25).
           05 TC                   PIC X(1).
           05 AMOUNT               PIC 9(5)V99.
      
       FD  OUTFILE.
       01  OUTREC                  PIC X(80).
      
       WORKING-STORAGE SECTION. 
       01  WS-EOF                  PIC X(3) VALUE 'NO '.
       01  WS-BAL                  PIC 9(7)V99 VALUE 0.
       01  WS-TACCNO               PIC X(3) VALUE SPACES.
       01  WS-TACCNAME             PIC X(25) VALUE SPACES.
       01  WS-DCTR                 PIC 9(5) VALUE 0.
       01  WS-BCTR                 PIC 9(9)V99 VALUE 0.
      
       01  REC-OUT.
           05 FILLER               PIC X(12) VALUE SPACES. 
           05 ACCNO-OUT            PIC X(3).
           05 FILLER               PIC X(13) VALUE SPACES.
           05 ACCNAME-OUT          PIC X(25). 
           05 FILLER               PIC X(4) VALUE SPACES.
           05 BAL-OUT              PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(12) VALUE SPACES.
       
       01  HEADER-LINE-1.
           05 FILLER               PIC X(30) VALUE SPACES.
           05 FILLER               PIC X(26) 
              VALUE "Account Transaction Report".
           05 FILLER               PIC X(24) VALUE SPACES.
       
       01  HEADER-LINE-2.
           05 FILLER               PIC X(80) VALUE SPACES. 
       
       01  HEADER-LINE-3.
           05 FILLER               PIC X(12) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE "Account No.".
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE "Account Name".
           05 FILLER               PIC X(18) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE "Balance".
           05 FILLER               PIC X(10) VALUE SPACES.
           
       01  HEADER-LINE-4.
           05 FILLER               PIC X(80) VALUE SPACES.
       
       01  TOTDREC. 
           05 FILLER               PIC X(12) VALUE SPACES.
           05 FILLER               PIC X(18) VALUE "TOTAL DEPOSITORS: ".
           05 DCTR-OUT             PIC Z,ZZ9.
           05 FILLER               PIC X(46) VALUE SPACES.
       
       01  TOTBREC.
           05 FILLER               PIC X(12) VALUE SPACES.
           05 FILLER               PIC X(28) 
              VALUE "TOTAL ACCUMULATED BALANCES: ".
           05 BCTR-OUT             PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(29) VALUE SPACES. 
       
       PROCEDURE DIVISION. 
       MAIN-RTN. 
           PERFORM INITIAL-RTN.
           PERFORM PROCESS-RTN UNTIL WS-EOF = 'YES'
           PERFORM FINISH-RTN
           CLOSE INFILE
           CLOSE OUTFILE.
           
           STOP RUN.
       
       INITIAL-RTN.
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE.
           
           WRITE OUTREC FROM HEADER-LINE-1 AFTER ADVANCING PAGE
           WRITE OUTREC FROM HEADER-LINE-2 AFTER ADVANCING 2 LINES
           WRITE OUTREC FROM HEADER-LINE-3 AFTER ADVANCING 1 LINE
           WRITE OUTREC FROM HEADER-LINE-4 AFTER ADVANCING 1 LINE.
           
           READ INFILE
              AT END MOVE 'YES' TO WS-EOF
           END-READ
        
           IF WS-EOF NOT = 'YES'
              MOVE ACCNO-IN TO WS-TACCNO
              MOVE ACCNAME-IN TO WS-TACCNAME
           END-IF.
       
       PROCESS-RTN.
           PERFORM UNTIL WS-EOF = 'YES'
              IF ACCNO-IN = WS-TACCNO
                  IF TC = 'D'
                      ADD AMOUNT TO WS-BAL
                  ELSE 
                      SUBTRACT AMOUNT FROM WS-BAL
                  END-IF
                  
                  READ INFILE 
                    AT END MOVE 'YES' TO WS-EOF
                  END-READ

                  IF WS-EOF = 'YES'
                      EXIT PERFORM
                  END-IF
              ELSE 
                  PERFORM ACCNT-BREAK-RTN
         
                  IF TC = 'D'
                      ADD AMOUNT TO WS-BAL
                  ELSE 
                      SUBTRACT AMOUNT FROM WS-BAL
                  END-IF
                  MOVE ACCNO-IN TO WS-TACCNO
                  MOVE ACCNAME-IN TO WS-TACCNAME
         
                  READ INFILE 
                    AT END MOVE 'YES' TO WS-EOF
                  END-READ

                  IF WS-EOF = 'YES'
                       EXIT PERFORM
                  END-IF
              END-IF
           END-PERFORM.
       
       ACCNT-BREAK-RTN.
           MOVE WS-TACCNO TO ACCNO-OUT
           MOVE WS-TACCNAME TO ACCNAME-OUT
           MOVE WS-BAL TO BAL-OUT
       
           ADD 1 TO WS-DCTR
           ADD WS-BAL TO WS-BCTR
       
           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 2 LINES
      
           MOVE 0 TO WS-BAL.
       
       FINISH-RTN. 
           MOVE WS-DCTR TO DCTR-OUT
           MOVE WS-BCTR TO BCTR-OUT
       
           WRITE OUTREC FROM TOTDREC AFTER ADVANCING 2 LINES
           WRITE OUTREC FROM TOTBREC AFTER ADVANCING 1 LINE. 
           