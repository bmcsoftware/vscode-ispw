       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CWBWCOBX.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  THIS PROGRAM IS A QSAM COBOL DEMO PROGRAM USED FOR            *
      *  TOPAZ FOR TOTAL TEST TRAINING SESSIONS.                       *
      *                                                                *
      *  INPUT FILE  - EMPLOYEE WAGE INFORMATION                       *
      *              - STORED IN THE ECC SLCXCNTL FILE - TTTDATH       *
      *  OUTPUT FILE - EMPLOYEE COMPENSATION REPORT                    *
      *              - REGIONAL SALES REPORT                           *
      *                                                                *
      *  RUN JCL     - STORED IN THE ECC SLCXCNTL FILE - LAUNCHCX      *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO EMPFILE.
           SELECT REPORT-FILE   ASSIGN TO RPTFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  EMPLOYEE-RECORD            PIC X(80).
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(80).
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05  JUMP-SW                PIC X        VALUE 'N'.
               88  JUMPING                         VALUE 'Y'.
           05  EOF-SW                 PIC X        VALUE 'N'.
               88  END-OF-FILE                     VALUE 'Y'.
           05  REGION-ERROR-SW        PIC X        VALUE 'N'.
               88  INVALID-REGION                  VALUE 'Y'.
               88  VALID-REGION                    VALUE 'N'.
           05  PARM-ERROR-SW          PIC X        VALUE 'N'.
               88  BAD-PARM                        VALUE 'Y'.
               88  GOOD-PARM                       VALUE 'N'.
           05  END-OF-MONTH-SW        PIC X        VALUE 'N'.
               88  END-OF-MONTH                    VALUE 'Y'.
       01  COUNTERS.
           05  PAGE-COUNT             PIC 9(3)     VALUE 1.
           05  EMP-LINE-COUNT         PIC S99      VALUE +56.
           05  REG-LINE-COUNT         PIC S99      VALUE +56.
           05  START-NUMBER           PIC 999.
           05  RECORDS-READ           PIC 999      VALUE 0.
           05  NORTH-COUNT            PIC 9(2)     VALUE 0.
           05  SOUTH-COUNT            PIC 9(2)     VALUE 0.
           05  EAST-COUNT             PIC 9(2)     VALUE 0.
           05  WEST-COUNT             PIC 9(2)     VALUE 0.
       01  REGION-SUB                 PIC 9        VALUE 0.
       01  YRS-OF-SERVICE             PIC 99       VALUE 0.
       01  TODAYS-DATE                PIC X(6).
       01  DATE-FIELDS REDEFINES TODAYS-DATE.
           05  DATE-YY                PIC 9(2).
           05  DATE-MM                PIC 9(2).
           05  DATE-DD                PIC 9(2).
********
********  HOLD EMPLOYEE DETAIL PRINT LINES UNTIL READY TO PRINT
********  EMPLOYEE COMPENSATION REPORT.  THE DATA IS STORED BY
********  REGION AND THEN BY SEQUENCE IN EMPLOYEE FILE.
********
       01  HOLD-TABLE.
           05  HOLD-AREA        OCCURS 4 TIMES
                                INDEXED BY REG-IX.
               10  HOLD-LINE    OCCURS 20 TIMES
                                INDEXED BY HOLD-IX.
                   15  HOLD-NAME               PIC X(15).
                   15  HOLD-REGION             PIC X(5).
                   15  HOLD-TYPE               PIC X.
                   15  HOLD-YEARS              PIC 9(2).
                   15  HOLD-HIRE-DATE.
                       20  HOLD-HIRE-YY        PIC 9(2).
                       20  HOLD-HIRE-MM        PIC 9(2).
                       20  HOLD-HIRE-DD        PIC 9(2).
                   15  HOLD-WAGES              PIC 9(5)V99.
                   15  HOLD-OT                 PIC 9(5)V99.
                   15  HOLD-COMM               PIC 9(5)V99.
                   15  HOLD-TOTAL              PIC 9(5)V99.
********
********  STORES THE NAME OF EACH REGION
********
       01  REGION-NAME-TABLE.
           05  FILLER            PIC X(5)    VALUE 'NORTH'.
           05  FILLER            PIC X(5)    VALUE 'SOUTH'.
           05  FILLER            PIC X(5)    VALUE 'EAST '.
           05  FILLER            PIC X(5)    VALUE 'WEST '.
       01  REGION-TABLE     REDEFINES REGION-NAME-TABLE.
           05  REGION-ID         PIC X(5)  OCCURS 4 TIMES.
********
********  STORES REGIONAL INFORMATION THAT IS USED TO PRINT THE
********  REGIONAL SALES REPORT.  REGION SALES IS A SUM OF ALL SALES
********  FOR THE REGION AND IS USED TO CALCULATE MANAGER COMMISSION
********  THE COMMENT FIELD IS USED TO FLAG A REGION IF AN EMPLOYEE IN
********  THE REGION HAS 0 SALES.
********
       01  REGION-SALES-TABLE.
           05  REGION-DATA         OCCURS 4 TIMES.
               10  REGION-NAME       PIC X(5).
               10  REGION-MANAGER    PIC X(15).
               10  REGION-SALARY     PIC 9(4)V99.
               10  REGION-SALES      PIC 9(6)V99.
               10  REGION-COMMENT    PIC X(5).
********
********  FIELDS USED BY CALLED PROGRAM CWBWSUBC TO CALCULATE
********  COMMISSION BASED ON SALES AMOUNT
********
       01  CALC-COMMISSION-FIELDS.
           05  EMP-TYPE              PIC X.
           05  CALC-SALES            PIC 9(6)V99           VALUE 0.
           05  CALC-COMMISSION       PIC 9(5)V99  COMP-3   VALUE 0.
********
********  ACCUMULATORS USED FOR CALCULATING HOURLY EMPLOYEE WAGES,
********  TOTAL EMPLOYEE COMPENSATION (SALARY PLUS COMMISSION OR
********  HOURLY EMPLOYEE WAGES PLUS OVERTIME), AND TOTAL MANAGEMENT
********  COMPENSATION (SALARY PLUS COMMISSION BASED ON TOTAL SALES
********  FOR THE REGION)
********
       01  TOTAL-FIELDS.
           05  EMP-WAGES             PIC 9(5)V99    COMP-3.
           05  EMP-COMPENSATION      PIC 9(5)V99    COMP-3.
           05  MGMT-COMPENSATION     PIC 9(5)V99    COMP-3.
********
********  TOTAL COMPENSATION GIVEN TO ALL EMPLOYEES (HOURLY AND SALES)
********  OR MANAGEMENT.  EACH SUM IS PRINTED AT THE END OF THEIR
********  RESPECTIVE REPORTS.
********
       01  GRAND-TOTAL-FIELDS.
           05  GRAND-TOTAL-EMP       PIC 9(7)V99   COMP-3  VALUE 0.
           05  GRAND-TOTAL-MGMT      PIC 9(7)V99   COMP-3  VALUE 0.
********
********  USED FOR CALCULATING OVERTIME FOR ANY HOURLY EMPLOYEE
********  WHOSE HOURS EXCEEDS 40
********
       01  OVERTIME-FIELDS.
           05  OT-AMOUNT             PIC 9(5)V99    COMP-3.
           05  OT-HOURS              PIC 9(2).
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
*********
*********  EMPLOYEE COMPENSATION REPORT
*********
       01  EMPLOYEE-HDR1.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(10)
                              VALUE  'RUN DATE  '.
           05  EMP-RUN-MM
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-DD
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-YY
                           PIC 99.
           05  FILLER      PIC X(7)      VALUE SPACES.
           05  FILLER      PIC X(28)
                              VALUE  'EMPLOYEE COMPENSATION REPORT'.
           05  FILLER      PIC X(18)     VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'PAGE '.
           05  EMP-PAGE    PIC ZZ9.
       01  EMPLOYEE-HDR2.
           05  FILLER      PIC X(31)     VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'HIRE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'YRS OF'.
           05  FILLER      PIC X(37)     VALUE SPACES.
       01  EMPLOYEE-HDR3.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(13)     VALUE 'EMPLOYEE NAME'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'REGION'.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'TYPE'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'DATE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(7)      VALUE 'SERVICE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'SALARY'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(3)      VALUE 'O/T'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(10)     VALUE 'COMMISSION'.
           05  FILLER      PIC X(4)      VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'TOTAL'.
       01  EMPLOYEE-DTL.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-NAME            PIC X(15).
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-REGION          PIC X(5).
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-TYPE            PIC X.
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-HIRE-MM         PIC 9(2).
           05  EMP-DTL-SLASH1          PIC X         VALUE SPACES.
           05  EMP-DTL-HIRE-DD         PIC 9(2).
           05  EMP-DTL-SLASH2          PIC X         VALUE SPACES.
           05  EMP-DTL-HIRE-YY         PIC 9(2).
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-YRS-OF-SERVICE  PIC 9(2).
           05  FILLER                  PIC X(2)      VALUE SPACES.
           05  EMP-DTL-WAGES           PIC ZZZZ9V99.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-OT              PIC ZZZZ9V99.
           05  FILLER                  PIC X(2)      VALUE SPACES.
           05  EMP-DTL-COMM            PIC ZZZZ9V99.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-TOTAL           PIC ZZZZ9V99.
       01  EMP-TOTAL-DTL.
           05  FILLER            PIC X(4)      VALUE SPACES.
           05  FILLER            PIC X(5)      VALUE 'TOTAL'.
           05  FILLER            PIC X(61)     VALUE SPACES.
           05  EMP-GRAND-TOTAL   PIC ZZZZZZ9V99.
*********
*********  REGIONAL SALES REPORT
*********
       01  REGION-HDR1.
           05  FILLER      PIC X      VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'RUN DATE  '.
           05  REG-RUN-MONTH
                           PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-DAY PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-YEAR
                           PIC 99.
           05  FILLER      PIC X(11)  VALUE SPACES.
           05  FILLER      PIC X(21)  VALUE  'REGIONAL SALES REPORT'.
           05  FILLER      PIC X(21)  VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'PAGE '.
           05  REG-PAGE    PIC ZZ9.
       01  REGION-HDR2.
           05  FILLER      PIC X      VALUE SPACES.
           05  FILLER      PIC X(7)   VALUE 'MANAGER'.
           05  FILLER      PIC X(9)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'REGION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(11)  VALUE 'TOTAL SALES'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'SALARY'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'COMMISSION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'TOTAL'.
           05  FILLER      PIC X(9)   VALUE SPACES.
       01  REGION-DETAIL.
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-MANAGER    PIC X(15).
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-REGION     PIC X(5).
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-SALES      PIC ZZZZZ9V99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-SALARY     PIC ZZZ9.99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-COMM       PIC ZZZZ9V99.
           05  FILLER             PIC X(3)      VALUE SPACES.
           05  REG-DTL-TOTAL      PIC ZZZZ9V99.
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-COMMENT    PIC X(5).
       01  MGMT-TOTAL-DTL.
           05  FILLER             PIC X(4)      VALUE SPACES.
           05  FILLER             PIC X(5)      VALUE 'TOTAL'.
           05  FILLER             PIC X(53)     VALUE SPACES.
           05  MGMT-GRAND-TOTAL   PIC ZZZZZZ9V99.
           05  FILLER             PIC X(8)      VALUE SPACES.
*********
*********  ERROR MESSAGE LINE
*********
       01  ERROR-LINE             PIC X(80).
*********
*********  BLANK LINE TO CONTROL SPACING OF REPORTS
*********
       01  BLANK-LINE             PIC X(80)   VALUE SPACES.
*********
*********  PARM IS AN OPTIONAL FIELD USED TO START PROCESSING
*********  AT A PARTICULAR RECORD IN THE EMPLOYEE FILE.  VALID
*********  VALUES FOR PARM-DATA ARE:
*********
*********        VALUE           FUNCTION
*********     - 00001            BEGIN PROCESSING FROM FIRST RECORD.
*********     - 00002            BEGIN PROCESSING FROM SECOND RECORD.
*********
       LINKAGE SECTION.
       01  PARMINFO.
           03  PARM-LTH          PIC S9(4) COMP.
           03  PARM-DATA          PIC X(5).
           03  FILLER             PIC X.
           03  RUN-DATE           PIC 9(6).
           03  GRAND-TOTAL        PIC S9(7) COMP.
           03  TOTAL-RECORD       PIC 999      VALUE 0.
       PROCEDURE DIVISION USING PARMINFO.
       0000-MAINLINE.
           PERFORM 9000-OPEN.
           PERFORM 9100-CHECK-PARM.
           IF GOOD-PARM
              PERFORM 9200-INIT
              PERFORM 8000-READ-INPUT
                   UNTIL RECORDS-READ = START-NUMBER
                   OR END-OF-FILE
              PERFORM 1000-PROCESS-DATA
                    UNTIL END-OF-FILE
              PERFORM 6000-PRINT-EMPLOYEE-REPORT
              MOVE 1 TO REGION-SUB
              IF END-OF-MONTH
                  PERFORM 7000-PRINT-REGION-REPORT
                        UNTIL REGION-SUB > 4.
              MOVE RECORDS-READ TO TOTAL-RECORD.
              MOVE EMP-GRAND-TOTAL TO GRAND-TOTAL.
           PERFORM 9900-CLOSE.
           GOBACK.
*********
*********  DIFFERENT PROCESSING OCCURS BASED ON EMPLOYEE TYPE.  THERE
*********  ARE 3 VALID EMPLOYEE TYPES.  IF A RECORD IS READ CONTAINING
*********  A RECORD TYPE OTHER THAN H, S OR M, AN ERROR MESSAGE IS
*********  WRITTEN AND PROCESSING CONTINUES.
*********
       1000-PROCESS-DATA.
           IF HOURLY
               PERFORM 2000-PROCESS-HOURLY
           ELSE
               IF SALES
                   PERFORM 3000-PROCESS-SALES
               ELSE
                   IF MANAGEMENT
                       PERFORM 4000-PROCESS-MANAGEMENT
                   ELSE
                       MOVE ' INVALID EMPLOYEE TYPE ' TO ERROR-LINE
                       WRITE REPORT-RECORD FROM ERROR-LINE.
           PERFORM 8000-READ-INPUT.
*********
*********  CALCULATE TYPE H (HOURLY) EMPLOYEE COMPENSATION.  ANY
*********  EMPLOYEE WITH MORE THAN 40 HOURS RECEIVES OVERTIME COMPUTED
*********  AT 1.5 TIMES THEIR HOURLY RATE.  ONCE EMPLOYEE COMPENSATION
*********  IS CALCULATED, IT IS STORED IN A HOLD TABLE.  THE DATA IN
*********  THE HOLD TABLE IS USED FOR PRINTING THE EMPLOYEE COMPENSATION
*********  REPORT.
*********
       2000-PROCESS-HOURLY.
           MOVE ZERO TO OT-AMOUNT.
           IF WA-EMP-HOURS GREATER THAN 40
               COMPUTE EMP-WAGES = WA-EMP-RATE * 40
               COMPUTE OT-HOURS  = WA-EMP-HOURS - 40
               COMPUTE OT-AMOUNT = OT-HOURS * (WA-EMP-RATE * 1.5)
           ELSE
               COMPUTE EMP-WAGES = WA-EMP-HOURS * WA-EMP-RATE.
           COMPUTE EMP-COMPENSATION = EMP-WAGES + OT-AMOUNT.
           ADD EMP-COMPENSATION TO GRAND-TOTAL-EMP.
           CALL 'CWBWDATE' USING END-OF-MONTH-SW
                                 YRS-OF-SERVICE
                                 TODAYS-DATE
                                 WA-EMP-HIRE-DATE.
           PERFORM 5000-STORE-EMPLOYEE-DETAIL.
           SET HOLD-IX UP BY 1.
*********
*********  CALCULATE TYPE S (SALES) EMPLOYEE COMPENSATION.  THE TOTAL
*********  SALES FOR THE EMPLOYEE IS PASSED TO THE CALLED PROGRAM WHICH
*********  CALCULATES COMMISSION.  ONCE EMPLOYEE COMPENSATION IS
*********  RETURNED FROM CWBWSUBC, IT IS STORED IN A HOLD-TABLE.  THE
*********  DATA IN THE HOLD-TABLE IS USED FOR PRINTING THE EMPLOYEE
*********  COMPENSATION REPORT.
*********
       3000-PROCESS-SALES.
           IF WA-SALES-AMOUNT > 0
              ADD  WA-SALES-AMOUNT  TO REGION-SALES (WA-EMP-REGION)
              MOVE WA-SALES-AMOUNT  TO CALC-SALES
              MOVE 0 TO CALC-COMMISSION
              MOVE 'S' TO EMP-TYPE
              CALL 'CWBWSUBC' USING EMP-TYPE,
                                    CALC-SALES,
                                    CALC-COMMISSION
           ELSE
              MOVE 'UH-OH' TO REGION-COMMENT (WA-EMP-REGION).
           COMPUTE EMP-COMPENSATION = WA-SALES-SALARY +
                                      CALC-COMMISSION.
           ADD  EMP-COMPENSATION TO GRAND-TOTAL-EMP.
           CALL 'CWBWDATE' USING END-OF-MONTH-SW
                                 YRS-OF-SERVICE
                                 TODAYS-DATE
                                 WA-EMP-HIRE-DATE.
           PERFORM 5000-STORE-EMPLOYEE-DETAIL.
           SET HOLD-IX UP BY 1.
*********
*********  PROCESS TYPE M (MANAGEMENT) RECORDS.  THE MANAGER NAME AND
*********  SALARY ARE STORED IN A TABLE FOR USE DURING PRINTING OF THE
*********  REGIONAL SALES REPORT.
*********
       4000-PROCESS-MANAGEMENT.
           MOVE WA-EMP-NAME    TO REGION-MANAGER (WA-EMP-REGION).
           MOVE WA-MGMT-SALARY TO REGION-SALARY (WA-EMP-REGION).
*********
*********  SALES AND HOURLY EMPLOYEE DATA IS STORED IN A HOLD TABLE FOR
*********  PRINTING OF EMPLOYEE COMPENSATION REPORT.  THE HOLD TABLE IS
*********  A TWO-DIMENSIONAL TABLE AND HOLDS DATA FOR A MAXIMUM OF 20
*********  EMPLOYEES.
*********
       5000-STORE-EMPLOYEE-DETAIL.
           PERFORM 5100-SET-INDEX.
           IF VALID-REGION
               MOVE WA-EMP-NAME TO HOLD-NAME (REG-IX, HOLD-IX)
               MOVE REGION-ID (WA-EMP-REGION)
                                TO HOLD-REGION (REG-IX, HOLD-IX)
               MOVE WA-EMP-TYPE TO HOLD-TYPE (REG-IX, HOLD-IX)
               MOVE WA-EMP-HIRE-DATE TO HOLD-HIRE-DATE (REG-IX, HOLD-IX)
               MOVE YRS-OF-SERVICE TO HOLD-YEARS (REG-IX, HOLD-IX)
               MOVE EMP-COMPENSATION
                                TO HOLD-TOTAL (REG-IX, HOLD-IX)
               IF HOURLY
                  MOVE EMP-WAGES TO HOLD-WAGES (REG-IX, HOLD-IX)
                  MOVE OT-AMOUNT TO HOLD-OT (REG-IX, HOLD-IX)
                  MOVE ZEROS     TO HOLD-COMM (REG-IX, HOLD-IX)
               ELSE
                  MOVE WA-SALES-SALARY
                                 TO HOLD-WAGES(REG-IX, HOLD-IX)
                  MOVE CALC-COMMISSION
                                 TO HOLD-COMM (REG-IX, HOLD-IX)
                  MOVE ZERO      TO HOLD-OT   (REG-IX, HOLD-IX).
*********
*********  SET THE REGION INDEX BASED ON EMPLOYEE REGION ID AND
*********  SEQUENTIALLY INCREMENT HOLD INDEX WITHIN EACH REGION.
*********  THE EMPLOYEE COMPENSATION REPORT WILL BE GROUPED BY
*********  REGION.  IF AN INVALID REGION IS FOUND, AN ERROR MESSAGE
*********  IS WRITTEN AND PROCESSING CONTINUES.
*********
       5100-SET-INDEX.
           MOVE 'N' TO REGION-ERROR-SW.
           IF NORTH
               ADD 1 TO NORTH-COUNT
               SET HOLD-IX TO NORTH-COUNT
           ELSE
               IF SOUTH
                   ADD 1 TO SOUTH-COUNT
                   SET HOLD-IX TO SOUTH-COUNT
               ELSE
                   IF EAST
                       ADD 1 TO EAST-COUNT
                       SET HOLD-IX TO EAST-COUNT
                   ELSE
                       IF WEST
                           ADD 1 TO WEST-COUNT
                           SET HOLD-IX TO WEST-COUNT
                       ELSE
                           MOVE 'Y' TO REGION-ERROR-SW.
           IF VALID-REGION
               SET REG-IX TO WA-EMP-REGION.
*********
*********  COMPENSATION DATA FOR HOURLY AND SALES EMPLOYEES ARE PRINTED
*********  TO THE EMPLOYEE COMPENSATION REPORT FROM THE HOLD TABLE.
*********
       6000-PRINT-EMPLOYEE-REPORT.
           SET REG-IX TO 1.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > NORTH-COUNT.
           SET REG-IX TO 2.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > SOUTH-COUNT.
           SET REG-IX TO 3.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > EAST-COUNT.
           SET REG-IX TO 4.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > WEST-COUNT.
           WRITE REPORT-RECORD FROM BLANK-LINE.
           MOVE GRAND-TOTAL-EMP TO EMP-GRAND-TOTAL.
           WRITE REPORT-RECORD FROM EMP-TOTAL-DTL.
*********
*********  PRINT DETAIL LINES FOR EMPLOYEE COMPENSATION REPORT
*********
       6100-PRINT-EMPLOYEE-DETAIL.
           IF EMP-LINE-COUNT GREATER THAN 55
                 PERFORM 6200-PRINT-EMPLOYEE-HEADERS.
           MOVE HOLD-NAME   (REG-IX, HOLD-IX) TO EMP-DTL-NAME.
           MOVE HOLD-REGION (REG-IX, HOLD-IX) TO EMP-DTL-REGION.
           MOVE HOLD-TYPE   (REG-IX, HOLD-IX) TO EMP-DTL-TYPE.
           MOVE HOLD-HIRE-MM(REG-IX, HOLD-IX) TO EMP-DTL-HIRE-MM.
           MOVE '/'                           TO EMP-DTL-SLASH1.
           MOVE HOLD-HIRE-DD(REG-IX, HOLD-IX) TO EMP-DTL-HIRE-DD.
           MOVE '/'                           TO EMP-DTL-SLASH2.
           MOVE HOLD-HIRE-YY(REG-IX, HOLD-IX) TO EMP-DTL-HIRE-YY.
           MOVE HOLD-YEARS  (REG-IX, HOLD-IX) TO EMP-DTL-YRS-OF-SERVICE.
           MOVE HOLD-WAGES  (REG-IX, HOLD-IX) TO EMP-DTL-WAGES.
           MOVE HOLD-OT     (REG-IX, HOLD-IX) TO EMP-DTL-OT.
           MOVE HOLD-COMM   (REG-IX, HOLD-IX) TO EMP-DTL-COMM.
           MOVE HOLD-TOTAL  (REG-IX, HOLD-IX) TO EMP-DTL-TOTAL.
           WRITE REPORT-RECORD FROM EMPLOYEE-DTL
             AFTER ADVANCING 1 LINE.
           ADD  1 TO EMP-LINE-COUNT.
           MOVE SPACES TO EMPLOYEE-DTL.
*********
*********  PRINT HEADERS FOR EMPLOYEE COMPENSATION REPORT
*********
       6200-PRINT-EMPLOYEE-HEADERS.
               MOVE PAGE-COUNT TO EMP-PAGE.
               MOVE DATE-YY TO EMP-RUN-YY.
               MOVE DATE-MM TO EMP-RUN-MM.
               MOVE DATE-DD TO EMP-RUN-DD.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM EMPLOYEE-HDR1.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM EMPLOYEE-HDR2.
               WRITE REPORT-RECORD FROM EMPLOYEE-HDR3.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               ADD 1 TO PAGE-COUNT.
               MOVE 5 TO EMP-LINE-COUNT.
*********
*********  COMPENSATION DATA FOR MANAGEMENT EMPLOYEES IS PRINTED
*********  TO THE REGIONAL SALES REPORT.  THE TOTAL SALES FOR THE
*********  REGION IS PASSED TO THE CALLED PROGRAM WHICH CALCULATES
*********  COMMISSION.  THIS REPORT IS GENERATED AT END-OF-MONTH.
*********
       7000-PRINT-REGION-REPORT.
           IF REG-LINE-COUNT GREATER THAN 55
               PERFORM 7100-PRINT-REGION-HEADERS.
           MOVE SPACES TO REGION-DETAIL.
           MOVE REGION-NAME    (REGION-SUB) TO REG-DTL-REGION.
           MOVE REGION-MANAGER (REGION-SUB) TO REG-DTL-MANAGER.
           MOVE REGION-SALARY  (REGION-SUB) TO REG-DTL-SALARY.
           MOVE REGION-SALES   (REGION-SUB) TO REG-DTL-SALES.
           MOVE REGION-SALES   (REGION-SUB) TO CALC-SALES.
           MOVE REGION-COMMENT (REGION-SUB) TO REG-DTL-COMMENT.
           MOVE 'M' TO EMP-TYPE.
           CALL 'CWBWSUBC' USING EMP-TYPE,
                                 CALC-SALES,
                                 CALC-COMMISSION.
           MOVE CALC-COMMISSION TO REG-DTL-COMM.
           COMPUTE MGMT-COMPENSATION = CALC-COMMISSION +
                                      REGION-SALARY(REGION-SUB).
           ADD  MGMT-COMPENSATION TO GRAND-TOTAL-MGMT.
           MOVE MGMT-COMPENSATION TO REG-DTL-TOTAL.
           WRITE REPORT-RECORD FROM REGION-DETAIL.
           IF REGION-SUB = 4
              WRITE REPORT-RECORD FROM BLANK-LINE
              MOVE GRAND-TOTAL-MGMT TO MGMT-GRAND-TOTAL
              WRITE REPORT-RECORD FROM MGMT-TOTAL-DTL.
           ADD 1 TO REG-LINE-COUNT.
           ADD 1 TO REGION-SUB.
*********
*********  PRINT HEADERS FOR REGIONAL SALES REPORT
*********
       7100-PRINT-REGION-HEADERS.
               MOVE PAGE-COUNT TO REG-PAGE.
               MOVE DATE-YY TO REG-RUN-YEAR.
               MOVE DATE-MM TO REG-RUN-MONTH.
               MOVE DATE-DD TO REG-RUN-DAY.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM REGION-HDR1.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               WRITE REPORT-RECORD FROM REGION-HDR2.
               WRITE REPORT-RECORD FROM BLANK-LINE.
               ADD 1 TO PAGE-COUNT.
               MOVE 4 TO REG-LINE-COUNT.
*********
*********
       8000-READ-INPUT.
           READ EMPLOYEE-FILE INTO EMPLOYEE-WORK-AREA
               AT END
                    MOVE 'Y' TO EOF-SW.
           ADD 1 TO RECORDS-READ.
*********
*********
       9000-OPEN.
           OPEN INPUT  EMPLOYEE-FILE.
           OPEN OUTPUT REPORT-FILE.
*********
*********  VALID PARMS ARE BLANK OR 5 DIGIT NUMBER
*********
       9100-CHECK-PARM.
           IF PARM-LTH = 12
               IF PARM-DATA IS NUMERIC
                   MOVE PARM-DATA TO START-NUMBER
               ELSE
                   PERFORM 9800-BAD-PARM
           ELSE
               IF PARM-LTH = 0
                   MOVE 1 TO START-NUMBER
               ELSE
                   PERFORM 9800-BAD-PARM.
*********
*********
       9200-INIT.
           MOVE 'NORTH' TO REGION-NAME (1).
           MOVE 'SOUTH' TO REGION-NAME (2).
           MOVE 'EAST ' TO REGION-NAME (3).
           MOVE 'WEST ' TO REGION-NAME (4).
           MOVE 1 TO REGION-SUB.
           PERFORM 9300-INITIALIZE-REGION-TABLE
              UNTIL REGION-SUB > 4.
           SET HOLD-IX TO 1.
           MOVE RUN-DATE TO TODAYS-DATE.
           CALL 'CWBWDATE' USING END-OF-MONTH-SW
                                 YRS-OF-SERVICE
                                 TODAYS-DATE
                                 WA-EMP-HIRE-DATE.
*********
*********
       9300-INITIALIZE-REGION-TABLE.
           MOVE SPACES TO REGION-MANAGER (REGION-SUB).
           MOVE SPACES TO REGION-COMMENT (REGION-SUB).
           MOVE 0 TO REGION-SALARY (REGION-SUB).
           MOVE 0 TO REGION-SALES  (REGION-SUB).
           ADD  1 TO REGION-SUB.
*********
*********
       9800-BAD-PARM.
           MOVE 'Y' TO PARM-ERROR-SW.
           MOVE '   PARAMETER LENGTH OR DATA IS INCORRECT   '
               TO ERROR-LINE.
           WRITE REPORT-RECORD FROM ERROR-LINE.
*********
*********
       9900-CLOSE.
           CLOSE EMPLOYEE-FILE.
           CLOSE REPORT-FILE.
*********
*********
       9999-RIP.
           DISPLAY '    ************    '.
           DISPLAY '   *            *   '.
           DISPLAY '  *   T H I S    *  '.
           DISPLAY ' *                * '.
           DISPLAY ' *      I S       * '.
           DISPLAY ' *                * '.
           DISPLAY ' *    D E A D     * '.
           DISPLAY ' *                * '.
           DISPLAY ' *    C O D E     * '.
           DISPLAY ' ****************** '.