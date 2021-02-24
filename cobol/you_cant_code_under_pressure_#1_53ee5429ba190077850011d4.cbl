123456*Indentation of at least 7 spaces is required for COBOL.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pressure.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC S9(8).
       01 Result PIC S9(8).

       PROCEDURE DIVISION.

       DoubleInteger.
       COMPUTE Result = N + N.

       END PROGRAM Pressure.
