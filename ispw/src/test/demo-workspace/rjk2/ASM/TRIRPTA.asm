TRIRPTA  CSECT
R1       EQU  1
R2       EQU  2
R3       EQU  3
R5       EQU  5
R6       EQU  6
R11      EQU  11
R12      EQU  12
R13      EQU  13
R14      EQU  14
R15      EQU  15
         USING *,R15
********************************************************
SETUP    EQU   *                   *
         STM   R14,R12,12(R13)     *    SAVE REGISTERS
         LR    R12,R15             *
         DROP  R15                 *    OPEN UP
         USING TRIRPTA,R12         *
         LA    R11,SAVEAREA        *
         ST    R13,4(R11)          *    CHAIN SAVE AREAS
         ST    R11,8(R13)          *
         LR    R13,R11             *
********************************************************
*********   START INSTRUCTIONS  ************************
START    EQU   *
         L     R2,0(R1)                   POINT TO PARAMETER LIST
         LR    R6,R2                      SAVE ADDRESS IN R6
         LA    R3,TRI1                    POINT R3 TO TITLES
         LA    R5,4                       SET COUNTER IN R5
         USING TYPES,R2                   SET ADDRESS FOR DSECT
         OPEN  (OUTDCB,OUTPUT)            OPEN OUTPUT FILE
         PUT   OUTDCB,TITLE               WRITE TITLE
         PUT   OUTDCB,BLANK               WRITE BLANK LINE
BLDLINE  MVC   NUMBER,PATTERN             MOVE EDIT PATTERN TO NUMBER
         ED    NUMBER,0(R2)               SET # OF TRIANGLES
         MVC   TRITYP,0(R3)               MOVE TITLE TO PRINTLN
PRINT    PUT   OUTDCB,PRINTLN             PRINT PRINTLN
         LA    R2,2(,R2)                  BUMP POINTER TO NEXT TYPE
         LA    R3,33(,R3)                 BUMP POINTER TO NEXT TITLE
         MVC   NUMBER,=XL2'F0'            ZERO OUT FIELD
GOBACK   BCT   R5,BLDLINE                 DO IT AGAIN
         PUT   OUTDCB,BLANK               WRITE BLANK LINE
         LR    R2,R6                      POINT R2 TO PARAMETER LIST
         AP    TYPEI,TYPEII               SUM # OF TRIANGLES
         AP    TYPEI,TYPEIII               "
         AP    TYPEI,TYPEIV                "
         MVC   NUMBER,PATTERN             MOVE EDIT PATTERN TO NUMBER
         ED    NUMBER,0(R2)               SET # OF TRIANGLES
         MVC   TRITYP,0(R3)               MOVE TITLE TO PRINTLN
         PUT   OUTDCB,PRINTLN             PRINT PRINTLN
         CLOSE OUTDCB                     CLOSE OUTPUT FILE
*********   END   INSTRUCTIONS  ************************
********************************************************
         SR    R15,R15              *
         L     R13,4(R13)           *
         LM    R14,R12,12(R13)      *    CLOSE UP
         BR    R14                  *
********************************************************
*********   DEFINITIONS   ******************************
********************************************************
SAVEAREA DS    0D
         DC    72X'0'
*********     DEFINITIONS     **************************
********************************************************
OUTDCB   DCB DSORG=PS,MACRF=(PM),DDNAME=OUTFILE,                       X
               RECFM=FB,LRECL=80
********************************************************
TITLE    DC  CL33'          *** TRIANGLE REPORT ***'
BLANK    DC  80C' '
TRI1     DC  CL33'EQUILATERAL TRIANGLES'
TRI2     DC  CL33'ISOSCELES TRIANGLES  '
TRI3     DC  CL33'SCALENE TRIANGLES    '
TRI4     DC  CL33'INVALID TRIANGLES    '
TRI#S    DC  CL33'INPUT RECORDS        '
PRINTLN  DS  0CL80
         DC  CL11'NUMBER OF  '
TRITYP   DC  CL33' '
NUMBER   DC  ZL4'0'
         DC  CL34' '
PATTERN  DC  XL4'40202120'
********************************************************
TYPES    DSECT
         DS  0CL6
TYPEI    DS  PL2
TYPEII   DS  PL2
TYPEIII  DS  PL2
TYPEIV   DS  PL2
         END TRIRPTA