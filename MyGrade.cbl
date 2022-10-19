       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MYGRADE.
       AUTHOR. 62160246.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT GRADE-FILE ASSIGN TO "mygrade.txt" 
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RESULT-FILE ASSIGN TO "avg.txt"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION. 
       FILE SECTION. 
       FD  GRADE-FILE.
       01 GRADE-DETAIL.
          88 END-OF-GRADE-FILE                VALUE HIGH-VALUE.
       05 GRADE-ID              PIC  9(6).
          05 GRADE-NAME         PIC X(50).
          05 GRADE-DEGREE       PIC 9(1).
          05 GRADE-GRADE        PIC X(2).
       FD RESULT-FILE.
       01 GRADE-AVG.
          05 AVG-GPA            PIC 9(2)V9(3).
          05 AVG-SCI-GPA        PIC 9(2)V9(3).
          05 AVG-CS-GPA         PIC 9(2)V9(3).

       WORKING-STORAGE SECTION. 
       01 AVG-GRADE.
          05 DEGREE             PIC 9(1).
          05 GRADE              PIC 9(1)V9(1).
          05 MUL-GRADE          PIC 9(2)V9(1).
       01 CAL-GRADE.
          05 SUM-DEGREE         PIC 9(3).
          05 SUM-GRADE          PIC 9(3)V9(2).
       01 AVG-SCI.
          05 CODE-SUBJ-SCI      PIC X(1).
          05 SUM-DEGREE-SCI     PIC 9(3).
          05 SUM-GRADE-SCI      PIC 9(3)V9(2).
       01 AVG-CS.
          05 CODE-SUBJ-CS       PIC X(2).
          05 SUM-DEGREE-CS      PIC 9(3).
          05 SUM-GRADE-CS       PIC 9(3)V9(2).

       PROCEDURE DIVISION.
       000-BEGIN.
           OPEN INPUT GRADE-FILE
           OPEN OUTPUT RESULT-FILE
           PERFORM UNTIL END-OF-GRADE-FILE 
                   READ GRADE-FILE 
                   AT END
                      SET END-OF-GRADE-FILE TO TRUE 
                   END-READ
                   IF NOT END-OF-GRADE-FILE THEN
                      PERFORM 001-PROCESS THRU 001-EXIT
                      PERFORM 002-PROCESS THRU 002-EXIT
                      PERFORM 003-PROCESS THRU 003-EXIT
                   END-IF 
      *            NitroEz.
           END-PERFORM
           COMPUTE AVG-GPA = SUM-GRADE / SUM-DEGREE 
           DISPLAY "AVG-GRADE : " AVG-GPA
           COMPUTE AVG-SCI-GPA = SUM-GRADE-SCI / SUM-DEGREE-SCI 
           DISPLAY "AVG-GRADE-SCI : " AVG-SCI-GPA 
           COMPUTE AVG-CS-GPA = SUM-GRADE-CS / SUM-DEGREE-CS
           DISPLAY "AVG-GRADE-CS : " AVG-CS-GPA 
           WRITE GRADE-AVG.
           CLOSE GRADE-FILE 
           CLOSE RESULT-FILE
           GOBACK.

       001-PROCESS.
           MOVE GRADE-DEGREE IN GRADE-DETAIL TO DEGREE IN AVG-GRADE
           
           EVALUATE TRUE 
           WHEN GRADE-GRADE = "A"
                MOVE 4 TO GRADE 
           WHEN GRADE-GRADE = "B+"
                MOVE 3.5 TO GRADE
           WHEN GRADE-GRADE = "B"
                MOVE 3 TO GRADE
           WHEN GRADE-GRADE = "C+"
                MOVE 2.5 TO GRADE
           WHEN GRADE-GRADE = "C"
                MOVE 2 TO GRADE
           WHEN GRADE-GRADE = "D+"
                MOVE 1.5 TO GRADE
           WHEN GRADE-GRADE = "D"
                MOVE 1 TO GRADE
           WHEN OTHER 
                MOVE 0 TO GRADE
           END-EVALUATE 
           COMPUTE MUL-GRADE = GRADE * DEGREE 
           COMPUTE SUM-DEGREE = SUM-DEGREE + DEGREE 
           COMPUTE SUM-GRADE = SUM-GRADE + MUL-GRADE 
      *    NitroEz.   
           .

       001-EXIT.
           EXIT.

       002-PROCESS.
           MOVE GRADE-ID IN GRADE-DETAIL TO CODE-SUBJ-SCI IN AVG-SCI 
           IF CODE-SUBJ-SCI = "3" THEN
              COMPUTE SUM-DEGREE-SCI = SUM-DEGREE-SCI + DEGREE 
              COMPUTE SUM-GRADE-SCI = SUM-GRADE-SCI + MUL-GRADE 
           END-IF 
           .

       002-EXIT.
           EXIT.

       003-PROCESS.
           MOVE GRADE-ID IN GRADE-DETAIL TO CODE-SUBJ-CS IN AVG-CS 
           IF CODE-SUBJ-CS = "31" THEN
              COMPUTE SUM-DEGREE-CS = SUM-DEGREE-CS + DEGREE 
              COMPUTE SUM-GRADE-CS = SUM-GRADE-CS + MUL-GRADE 
           END-IF 
           .

       003-EXIT.
           EXIT.