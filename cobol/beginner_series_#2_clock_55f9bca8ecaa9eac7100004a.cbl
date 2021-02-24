123456*Indentation of at least 7 spaces is required for COBOL.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Clock.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Input
       01 Hours        PIC 9(02).
       01 Minutes      PIC 9(02)
       01 Seconds      PIC 9(02).
      * Output
       01 Millis       PIC 9(08).

       PROCEDURE DIVISION.
       MOVE 0 TO Millis.
       Past SECTION.
          COMPUTE Millis = Seconds * 1000.
          COMPUTE Millis = Millis + Minutes * 60 * 1000
          COMPUTE Millis = Millis + Hours * 60 * 60 * 1000

       END PROGRAM Clock.
