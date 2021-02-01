TRITSTA  CSECT
R1       EQU   1
R2       EQU   2
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         USING *,R15
********************************************************
SETUP    EQU   *                   *
         STM   R14,R12,12(R13)     *    SAVE REGISTERS
         LR    R12,R15             *
         DROP  R15                 *    OPEN UP
         USING TRITSTA,R12         *
         LA    R11,SAVEAREA        *
         ST    R13,4(R11)          *    CHAIN SAVE AREAS
         ST    R11,8(R13)          *
         LR    R13,R11             *
********************************************************
*********   START INSTRUCTIONS  ************************
START    EQU   *
         L     R2,0(R1)         POINT R2 TO PARAMETER
         USING TSTREC,R2        SET ADDRESS FOR DSECT
ZEROIT   ZAP   ANB,=P'0'
         ZAP   ANC,=P'0'
         ZAP   BNC,=P'0'
PACKIT   PACK  TSTA,SIDEA       PACK TRIANGLE SIDE
         PACK  TSTB,SIDEB         "
         PACK  TSTC,SIDEC         "
ADDIT    MVC   ANB+1(1),TSTA    ADD SIDEA TO SIDEB GIVING ANB
         AP    ANB,TSTB
         MVC   ANC+1(1),TSTA    ADD SIDEA TO SIDEC GIVING ANC
         AP    ANC,TSTC
         MVC   BNC+1(1),TSTB    ADD SIDEB TO SIDEC GIVING BNC
         AP    BNC,TSTC
VALIDATE CP    BNC,TSTA         BNC NOT > A?
         BL    MOVEIT           THEN INVALID; GOTO MOVEIT
         CP    BNC,TSTA         BNC = A?
         BE    MOVEIT           THEN INVALID; GOTO MOVEIT
         CP    ANC,TSTB         ANC NOT > B?
         BL    MOVEIT           THEN INVALID; GOTO MOVEIT
         CP    ANC,TSTB         ANC = B?
         BE    MOVEIT           THEN INVALID; GOTO MOVEIT
         CP    ANB,TSTC         ANB NOT > C?
         BL    MOVEIT           THEN INVALID; GOTO MOVEIT
         CP    ANB,TSTC         ANB = C?
         BE    MOVEIT           THEN INVALID; GOTO MOVEIT
         B     TRICHK           TRIANGLE OK; CONTINUE
MOVEIT   MVI   TYPE,C'4'        SET TYPE TO INVALID TRIANGLE(4)
         B     ENDIT            RETURN TO READIT
TRICHK   CP    TSTA,TSTB        IS A = B?
         BE    EQUAL            IF SO, BRANCH EQUAL
         CP    TSTB,TSTC        OR IS B = C?
         BE    TYPEII           IF SO, BRANCH TYPEII
         CP    TSTA,TSTC        OR IS A = C?
         BE    TYPEII           IF SO, BRANCH TYPEII
         B     SCALENE          OTHERWISE BRANCH SCALENE
EQUAL    CP    TSTB,TSTC        IS B = C?
         BNE   TYPEII
         MVI   TYPE,C'1'        OTHERWISE SET TYPE TO 1(EQUILATERAL)
         B     ENDIT            RETURN TO READIT
SCALENE  MVI   TYPE,C'3'        SET TYPE TO SCALENE(3)
         B     ENDIT
TYPEII   MVI   TYPE,C'2'        SET TYPE TO ISOSCELES(2)
*********   END   INSTRUCTIONS  ************************
********************************************************
ENDIT    SR    R15,R15              *
         L     R13,4(R13)           *
         LM    R14,R12,12(R13)      *    CLOSE UP
         BR    R14                  *
********************************************************
*********   DEFINITIONS   ******************************
********************************************************
SAVEAREA DS    0D
         DC    72X'0'
********************************************************
         DS  0CL3
TSTA     DS  PL1
TSTB     DS  PL1
TSTC     DS  PL1
         DS  0CL6
ANB      DS  PL2
ANC      DS  PL2
BNC      DS  PL2
TSTREC   DSECT
TYPE     DS  ZL1
         DS  0CL3
SIDEA    DS  ZL1
SIDEB    DS  ZL1
SIDEC    DS  ZL1
         END TRITSTA