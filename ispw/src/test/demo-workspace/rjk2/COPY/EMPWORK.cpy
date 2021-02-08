********
********  EMPLOYEE RECORD WORK-AREA.  EMPLOYEE DATA IS REDEFINED
********  BASED ON ONE OF THE 3 EMPLOYEE TYPES, HOURLY, SALES OR
********  MANAGEMENT.
********
       01  EMPLOYEE-WORK-AREA.
           05  WA-EMP-NUM            PIC 9(5).
           05  WA-EMP-TYPE           PIC X.
               88  HOURLY            VALUE 'H'.
               88  SALES             VALUE 'S'.
               88  MANAGEMENT        VALUE 'M'.
           05  WA-EMP-REGION         PIC 9.
               88  NORTH             VALUE 1.
               88  SOUTH             VALUE 2.
               88  EAST              VALUE 3.
               88  WEST              VALUE 4.
           05  WA-EMP-NAME           PIC X(15).
           05  WA-EMP-ADDRESS.
               10  WA-EMP-STREET     PIC X(15).
               10  WA-EMP-CITY       PIC X(8).
               10  WA-EMP-STATE      PIC XX.
               10  WA-EMP-ZIP        PIC X(9).
           05  WA-HOURLY-EMPLOYEE-DATA.
               10  WA-EMP-HOURS      PIC 9(2).
               10  WA-EMP-RATE       PIC 9(3)V99     COMP-3.
               10  FILLER            PIC X(8).
           05  WA-SALES-EMPLOYEE-DATA   REDEFINES
                                        WA-HOURLY-EMPLOYEE-DATA.
               10  WA-SALES-SALARY   PIC 9(5)V99     COMP-3.
               10  WA-SALES-AMOUNT   PIC 9(5)V99.
               10  FILLER            PIC X(2).
           05  WA-MGMT-EMPLOYEE-DATA   REDEFINES
                                        WA-SALES-EMPLOYEE-DATA.
               10  WA-MGMT-SALARY    PIC 9(5)V99     COMP-3.
               10  FILLER            PIC X(9).
           05  WA-EMP-HIRE-DATE.
               10  WA-EMP-HIRE-YY    PIC 9(2).
               10  WA-EMP-HIRE-MM    PIC 9(2).
               10  WA-EMP-HIRE-DD    PIC 9(2).
           05  FILLER                PIC X(5).