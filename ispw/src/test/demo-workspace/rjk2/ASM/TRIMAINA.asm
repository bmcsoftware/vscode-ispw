TRIMAINA CSECT
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
         USING TRIMAINA,R12        *
         LA    R11,SAVEAREA        *
         ST    R13,4(R11)          *    CHAIN SAVE AREAS
         ST    R11,8(R13)          *
         LR    R13,R11             *
********************************************************
*********   START INSTRUCTIONS  ************************
START    EQU   *
         OPEN  (INDCB,INPUT)              OPEN INPUT FILE
         TM    INDCB+X'30',X'10'          OPEN SUCCESSFUL?
         BZ    ENDIT                      BRANCH TO ENDIT IF NOT OPEN
         ZAP   TYPEI,=P'0'
         ZAP   TYPEII,=P'0'
         ZAP   TYPEIII,=P'0'
         ZAP   TYPEIV,=P'0'
AGAIN    GET   INDCB,INREC                READ A RECORD
         MVI   TYPE,C'0'                  ZERO OUT FIELD 'TYPE'
         CALL  TRITSTA,(TYPE)             CALL SUBORDINATE PROGRAM
         CLI   TYPE,C'1'                  IS TYPE = 1?
         BE    EQUAL
         CLI   TYPE,C'2'                  IS TYPE = 2?
         BE    ISOS
         CLI   TYPE,C'3'                  IS TYPE = 3?
         BE    SCALENE
         CLI   TYPE,C'4'                  IS TYPE = 4?
         BE    INVALID
         B     AGAIN                      BRANCH TO READ
EQUAL    AP    TYPEI,=P'1'                ADD 1 TO TYPEI
         B     AGAIN
ISOS     AP    TYPEII,=P'1'               ADD 1 TO TYPEII
         B     AGAIN
SCALENE  AP    TYPEIII,=P'1'              ADD 1 TO TYPEIII
         B     AGAIN
INVALID  AP    TYPEIV,=P'1'               ADD 1 TO TYPEIV
         B     AGAIN
EOD      CALL  TRIRPTA,(TYPEI)            CALL REPORT WRITER
ENDIT    CLOSE INDCB                      CLOSE INPUT FILE
*********   END   INSTRUCTIONS  ************************
********************************************************
         L     R13,4(R13)           *
         LM    R14,R12,12(R13)      *    CLOSE UP
         SR    R15,R15              *    CLEAR R15
         BR    R14                  *
********************************************************
*********   DEFINITIONS   ******************************
********************************************************
SAVEAREA DS    0D
         DC    72X'0'
********************************************************
INDCB    DCB DSORG=PS,MACRF=(GM),DDNAME=INFILE,EODAD=EOD,              X
               RECFM=FB,LRECL=80
********************************************************
TESTFLD  DS    PL1
********************************************************
TYPEI    DS    PL2          |
TYPEII   DS    PL2          | THESE FIELDS PASSED TO DSECT
TYPEIII  DS    PL2          | IN SECOND SUBORDINATE MODULE.
TYPEIV   DS    PL2          |
********************************************************
TYPE     DS ZL1             |
INREC    DS 0CL80           | THESE FIELDS PASSED TO DSECT IN FIRST
SIDEA    DS ZL1             | SUBORDINATE MODULE:  DO NOT CHANGE
SIDEB    DS ZL1             | ORDER OR LENGTHS.
SIDEC    DS ZL1             |
FILLER   DS CL77            |
********************************************************
         END TRIMAINA