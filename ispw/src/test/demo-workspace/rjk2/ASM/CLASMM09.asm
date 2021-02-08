         TITLE 'SEE IF MEMBER IS IN THE PDS'
         SPACE 3
         CLEQUM09
BASEREG  EQU   12
CLASMM09 CSECT
*******************************************************************
**    SAVE REGISTERS, ETC.
*******************************************************************
         STM   R14,R12,12(R13)               SAVE REGISTERS R14 THRU 12
         LR    BASEREG,R15                   LOAD ENTRY POINT
         USING CLASMM09,BASEREG              ESTABLISH REGISTER
         LA    R4,SAVE                       GET ADDRESS OF SAVE AREA
         ST    R4,8(R13)                     CALLERS FORWARD CHAIN
         ST    R13,4(R4)                     MY BACKWARD CHAIN
         LR    R13,R4                        LOAD R13 TO MY SAVE AREA
         WTO   ' CLASMM09 Starting',ROUTCDE=(11),DESC=(7)
         CLAMAC09
         L     R15,=V(CLASMS09)
         BASR  R14,R15
         WTO   ' CLASMM09 Ending',ROUTCDE=(11),DESC=(7)
*******************************************************************
**  GO BACK TO CALLING PROGRAM
*******************************************************************
GOBACK   EQU   *
         L     R13,SAVE+4       RESTORE REG 13
         LM    R14,R12,12(R13)  RESTORE REGS 14 THRU 12
         SR    R15,R15          SET CONDCODE=0
         BR    R14              RETURN
*******************************************************************
* MAIN STORAGE
*******************************************************************
         LTORG
         DS    0F
SAVE     DS    18F
         END   CLASMM09