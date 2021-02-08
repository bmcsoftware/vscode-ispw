000100 IDENTIFICATION DIVISION.                                         00010006
000200 PROGRAM-ID. IB4OP01.                                             00020000
000300*                                                                 00030000
000400* THIS PROGRAM IS A BATCH DLI TRANSACTION PROCESSING PROGRAM.     00040000
000500*                                                                 00050000
000600 ENVIRONMENT DIVISION.                                            00060000
000700 CONFIGURATION SECTION.                                           00070000
000800 SOURCE-COMPUTER. IBM-370.                                        00080000
000900 OBJECT-COMPUTER. IBM-370.                                        00090000
001000 INPUT-OUTPUT SECTION.                                            00100000
001100 FILE-CONTROL.                                                    00110000
001200     SELECT TRANS-FILE ASSIGN TO UT-S-TRANS                       00120000
001300         FILE STATUS IS FS-TRANS.                                 00130003
001400     SELECT RPT-FILE ASSIGN TO UT-S-PRINT1                        00140003
001500         FILE STATUS IS FS-RPT.                                   00150003
001600     SELECT ERR-FILE ASSIGN TO UT-S-PRINT2                        00160000
001700         FILE STATUS IS FS-ERR.                                   00170002
001800 DATA DIVISION.                                                   00180000
001900 FILE SECTION.                                                    00190000
002000 FD  TRANS-FILE RECORDING MODE F BLOCK 0 RECORDS.                 00200000
002100     COPY TRANS.                                                  00210008
002200 FD  RPT-FILE RECORDING MODE F BLOCK 0 RECORDS.                   00220000
002300 01  RPT-REC.                                                     00230000
002400     03  R-CC               PIC X.                                00240005
002500     03  FILLER             PIC X(132).                           00250000
002600 FD  ERR-FILE RECORDING MODE F BLOCK 0 RECORDS.                   00260000
002700 01  ERR-REC.                                                     00270000
002800     03  E-CC               PIC X.                                00280005
002900     03  E-TRAN-ID          PIC X(12).                            00290000
003000     03  FILLER             PIC XX.                               00300000
003100     03  E-MESSAGE          PIC X(100).                           00310000
003200     03  FILLER             PIC X(18).                            00320007
003300                                                                  00330000
003400 WORKING-STORAGE SECTION.                                         00340000
003500 77  MAX-LINES           PIC S9999 COMP VALUE +55.                00350005
003600 77  CUR-LINE-CNT        PIC S9999 COMP VALUE +256.               00360005
003700 77  ERR-LINE-CNT        PIC S9999 COMP VALUE +256.               00370029
003800 77  ADV-LINES           PIC S9999 COMP VALUE +1.                 00380005
003900 77  LOGICAL-LINE        PIC S9999 COMP VALUE +1.                 00390015
004000 77  FS-TRANS            PIC 99.                                  00400003
004100 77  FS-RPT              PIC 99.                                  00410003
004200 77  FS-ERR              PIC 99.                                  00420003
004300 77  TRANS-EOF           PIC X VALUE ' '.                         00430003
004400     88  TRANS-PRESENT     VALUE ' '.                             00440003
004500     88  NO-MORE-TRANS     VALUE 'Y'.                             00450003
004600 77  TRANS-STATUS        PIC X.                                   00460003
004700     88  VALID-INCOMPL     VALUE ' '.                             00470003
004800     88  VALID-TRANS       VALUE 'V'.                             00480003
004900     88  TRAN-WARN         VALUE 'W'.                             00490003
005000     88  TRAN-ERR          VALUE 'E'.                             00500003
005100 77  ACTION              PIC XXXX.                                00510003
005200 01  PRT-DTL.                                                     00520005
005300     03  P-CC            PIC X.                                   00530004
005400     03  P-TRANS         PIC X(5).                                00540004
005500     03  FILLER          PIC X.                                   00550004
005600     03  P-ORD-NUM       PIC X(6).                                00560004
005700     03  FILLER          PIC X.                                   00570004
005800     03  P-ACT           PIC X(10).                               00580004
005900     03  FILLER          PIC X.                                   00590004
006000     03  P-F1-DTL.                                                00600004
006100         05 P-CUST-NO    PIC X(6).                                00610004
006200         05 FILLER       PIC X.                                   00620004
006300         05 P-DESCR      PIC X(40).                               00630004
006400         05 FILLER       PIC X.                                   00640004
006500         05 P-ORD-QTY    PIC ZZ,ZZ9.                              00650004
006600         05 P-ORD-QTY-X  REDEFINES P-ORD-QTY PIC X(6).            00660007
006700         05 FILLER       PIC X.                                   00670004
006800         05 P-ORD-AMT    PIC ZZ,ZZ9.99-.                          00680004
006900         05 P-ORD-AMT-X  REDEFINES P-ORD-AMT PIC X(10).           00690010
007000         05 FILLER       PIC X.                                   00700004
007100         05 P-TYPE       PIC XX.                                  00710004
007200         05 FILLER       PIC X.                                   00720004
007300         05 P-ORD-STAT   PIC 99.                                  00730004
007400         05 P-ORD-STAT-X REDEFINES P-ORD-STAT PIC XX.             00740007
007500         05 FILLER       PIC X.                                   00750004
007600         05 P-PRTY-CD    PIC X.                                   00760004
007700         05 FILLER       PIC X.                                   00770004
007800         05 P-ACT-ORD-QTY PIC ZZ,ZZ9.                             00780004
007900         05 P-ACT-ORD-QTY-X REDEFINES P-ACT-ORD-QTY PIC X(6).     00790007
008000         05 FILLER       PIC X.                                   00800004
008100         05 P-TOT-SCRAP  PIC ZZ,ZZ9.                              00810004
008200         05 P-TOT-SCRAP-X REDEFINES P-TOT-SCRAP PIC X(6).         00820007
008300         05 FILLER       PIC X(21).                               00830032
008400     03  P-F2-DTL REDEFINES P-F1-DTL.                             00840004
008500         05 FILLER       PIC X(7).                                00850004
008600         05 P-STRT-CAPT  PIC X(8).                                00860004
008700         05 P-UNIT-STRT  PIC ZZ,ZZ9.                              00870004
008800         05 P-UNIT-STRT-X REDEFINES P-UNIT-STRT PIC X(6).         00880007
008900         05 FILLER       PIC X.                                   00890004
009000         05 P-PCT-STRT   PIC ZZ,ZZ9.999-.                         00900004
009100         05 P-PCT-STRT-X REDEFINES P-PCT-STRT PIC X(11).          00910007
009200         05 P-PCT-CAPT   PIC XX.                                  00920004
009300         05 P-COMP-CAPT  PIC X(8).                                00930004
009400         05 P-UNIT-COMP  PIC ZZ,ZZ9.                              00940004
009500         05 P-UNIT-COMP-X REDEFINES P-UNIT-COMP PIC X(6).         00950010
009600         05 FILLER       PIC X.                                   00960004
009700         05 P-PCT-COMP   PIC ZZ,ZZ9.999-.                         00970004
009800         05 P-PCT-COMP-X REDEFINES P-PCT-COMP PIC X(11).          00980007
009900         05 P-PCT-CAPT2  PIC X.                                   00990004
010000         05 FILLER       PIC X(25).                               01000032
010100         05 P-DATE       PIC X(10).                               01010004
010200         05 FILLER       PIC X.                                   01020032
010300         05 P-DATE-L     PIC X(10).                               01030032
010400 01  ERR-DTL.                                                     01040005
010500     03  P-CC            PIC X.                                   01050007
010600     03  P-TRANS         PIC X(5).                                01060007
010700     03  FILLER          PIC X.                                   01070007
010800     03  P-ORD-NUM       PIC X(6).                                01080007
010900     03  FILLER          PIC X.                                   01090007
011000     03  P-ACT           PIC X(10).                               01100007
011100     03  FILLER          PIC X.                                   01110007
011200     03  P-F1-DTL.                                                01120007
011300         05 P-CUST-NO    PIC X(6).                                01130007
011400         05 FILLER       PIC X.                                   01140007
011500         05 P-DESCR      PIC X(40).                               01150007
011600         05 FILLER       PIC X.                                   01160007
011700         05 P-ORD-QTY    PIC ZZ,ZZ9.                              01170007
011800         05 P-ORD-QTY-X  REDEFINES P-ORD-QTY PIC X(6).            01180007
011900         05 FILLER       PIC X.                                   01190007
012000         05 P-ORD-AMT    PIC ZZ,ZZ9.99-.                          01200007
012100         05 P-ORD-AMT-X  REDEFINES P-ORD-AMT PIC X(10).           01210010
012200         05 FILLER       PIC X.                                   01220007
012300         05 P-TYPE       PIC XX.                                  01230007
012400         05 FILLER       PIC X.                                   01240007
012500         05 P-ORD-STAT   PIC 99.                                  01250007
012600         05 P-ORD-STAT-X REDEFINES P-ORD-STAT PIC XX.             01260007
012700         05 FILLER       PIC X.                                   01270007
012800         05 P-PRTY-CD    PIC X.                                   01280007
012900         05 FILLER       PIC X.                                   01290007
013000         05 P-ACT-ORD-QTY PIC ZZ,ZZ9.                             01300007
013100         05 P-ACT-ORD-QTY-X REDEFINES P-ACT-ORD-QTY PIC X(6).     01310007
013200         05 FILLER       PIC X.                                   01320007
013300         05 P-TOT-SCRAP  PIC ZZ,ZZ9.                              01330007
013400         05 P-TOT-SCRAP-X REDEFINES P-TOT-SCRAP PIC X(6).         01340007
013500         05 FILLER       PIC X(21).                               01350032
013600     03  P-F2-DTL REDEFINES P-F1-DTL.                             01360007
013700         05 FILLER       PIC X(7).                                01370007
013800         05 P-STRT-CAPT  PIC X(8).                                01380007
013900         05 P-UNIT-STRT  PIC ZZ,ZZ9.                              01390007
014000         05 P-UNIT-STRT-X REDEFINES P-UNIT-STRT PIC X(6).         01400007
014100         05 FILLER       PIC X.                                   01410007
014200         05 P-PCT-STRT   PIC ZZ,ZZ9.999-.                         01420007
014300         05 P-PCT-STRT-X REDEFINES P-PCT-STRT PIC X(11).          01430007
014400         05 P-PCT-CAPT   PIC XX.                                  01440007
014500         05 P-COMP-CAPT  PIC X(8).                                01450007
014600         05 P-UNIT-COMP  PIC ZZ,ZZ9.                              01460007
014700         05 P-UNIT-COMP-X REDEFINES P-UNIT-COMP PIC X(6).         01470010
014800         05 FILLER       PIC X.                                   01480007
014900         05 P-PCT-COMP   PIC ZZ,ZZ9.999-.                         01490007
015000         05 P-PCT-COMP-X REDEFINES P-PCT-COMP PIC X(11).          01500007
015100         05 P-PCT-CAPT2  PIC X.                                   01510007
015200         05 FILLER       PIC X(25).                               01520032
015300         05 P-DATE       PIC X(10).                               01530007
015400         05 FILLER       PIC X.                                   01540032
015500         05 P-DATE-L     PIC X(10).                               01550032
015600 01  H1.                                                          01560033
015700     03  FILLER             PIC X(45) VALUE IS                    01570029
015800     '                                             '.             01580030
015900     03  FILLER             PIC X(50) VALUE IS                    01590005
016000     ' ORDER PROCESSING                                 '.        01600005
016100     03  FILLER             PIC X(32) VALUE IS                    01610005
016200     '                                '.                          01620005
016300 01  H1-E.                                                        01630029
016400     03  FILLER             PIC X(40) VALUE IS                    01640029
016500     '                                        '.                  01650030
016600     03  FILLER             PIC X(50) VALUE IS                    01660029
016700     ' ORDER PROCESSING ERRORS                          '.        01670029
016800     03  FILLER             PIC X(32) VALUE IS                    01680029
016900     '                                '.                          01690029
017000 01  H2.                                                          01700005
017100     03  FILLER             PIC X(50) VALUE IS                    01710005
017200     ' TRANS  ORDR    ACTION   CUST#  * -------------- '.         01720029
017300     03  FILLER             PIC X(50) VALUE IS                    01730005
017400     'DESCRIPTION -------- *   QTY     AMT     TP       '.        01740005
017500     03  FILLER             PIC X(32) VALUE IS                    01750005
017600     '            FIRST ACT  LAST ACT '.                          01760032
017700 01  W-DATE-FMT.                                                  01770004
017800     03  W-YYYY          PIC X(4).                                01780004
017900     03  FILLER          PIC X VALUE '/'.                         01790004
018000     03  W-MM            PIC XX.                                  01800004
018100     03  FILLER          PIC X VALUE '/'.                         01810004
018200     03  W-DD            PIC XX.                                  01820004
018300 01  W-DATE.                                                      01830004
018400     03  W-YYYY          PIC XXXX.                                01840004
018500     03  W-MM            PIC XX.                                  01850004
018600     03  W-DD            PIC XX.                                  01860004
018700 01  SSA-1.                                                       01870008
018800     03  S1-SEG-NAME     PIC X(8) VALUE IS 'ORDR010 '.            01880013
018900     03  FILLER          PIC X VALUE '('.                         01890027
019000     03  S1-FLD-NAME     PIC X(8) VALUE 'ORDRKEY '.               01900026
019100     03  S1-SC-OP        PIC XX VALUE ' ='.                       01910026
019200     03  SSA-1-KEY       PIC X(6).                                01920012
019300     03  FILLER          PIC XX VALUE ') '.                       01930008
019400 01  SSA-2.                                                       01940032
019500     03  S2-SEG-NAME     PIC X(8) VALUE IS 'ORDR010 '.            01950032
019600     03  FILLER          PIC X VALUE ' '.                         01960032
019700     COPY ORDR.                                                   01970010
019800 LINKAGE SECTION.                                                 01980001
019900 01  DBPCB.                                                       01990038
020000     02  DBD-NAME        PIC  X(8).                               02000001
020100     02  SEG-LEVEL       PIC  X(2).                               02010002
020200     02  DBSTATUS        PIC  X(2).                               02020002
020300     02  PROC-OPTIONS    PIC  X(4).                               02030002
020400     02  RESERVE-DLI     PIC  X(4).                               02040002
020500     02  SEG-NAME-FB     PIC  X(8).                               02050002
020600     02  LENGTH-FB-KEY   PIC  9(4).                               02060002
020700     02  NUMB-SENS-SEGS  PIC  9(4).                               02070002
020800     02  KEY-FB-AREA     PIC  X(17).                              02080009
020900                                                                  02090001
021000 01  IOPCB.                                                       02100022
021100     02  I-DBD-NAME        PIC  X(8).                             02110022
021200     02  I-SEG-LEVEL       PIC  X(2).                             02120022
021300     02  I-DBSTATUS        PIC  X(2).                             02130022
021400     02  I-PROC-OPTIONS    PIC  X(4).                             02140022
021500     02  I-RESERVE-DLI     PIC  X(4).                             02150022
021600     02  I-SEG-NAME-FB     PIC  X(8).                             02160022
021700     02  I-LENGTH-FB-KEY   PIC  9(4).                             02170022
021800     02  I-NUMB-SENS-SEGS  PIC  9(4).                             02180022
021900     02  I-KEY-FB-AREA     PIC  X(17).                            02190022
022000                                                                  02200022
022100 PROCEDURE DIVISION USING IOPCB DBPCB.                            02210022
022200 I100-INIT.                                                       02220001
022300     DISPLAY 'ENTERING PROGRAM IB4OP01'                           02230038
022400     OPEN INPUT TRANS-FILE, OUTPUT RPT-FILE, ERR-FILE.            02240002
022500     IF FS-TRANS NOT = 0 OR FS-RPT NOT = 0 OR FS-ERR NOT = 0      02250010
022600         DISPLAY 'UNABLE TO OPEN ONE OF THE FILES, '              02260002
022700         DISPLAY '  PROGRAM TERMINATING.'                         02270002
022800         DISPLAY 'TRANSACTION FILE STATUS IS ' FS-TRANS           02280002
022900         DISPLAY 'REPORT FILE STATUS IS ' FS-RPT                  02290002
023000         DISPLAY 'ERROR FILE STATUS IS ' FS-ERR                   02300003
023100     END-IF                                                       02310002
023200     PERFORM R010-READ UNTIL NO-MORE-TRANS                        02320004
023300     CLOSE TRANS-FILE, RPT-FILE, ERR-FILE                         02330004
023400     DISPLAY 'LEAVING  PROGRAM IB4OP01'                           02340036
023500     GOBACK.                                                      02350004
023600                                                                  02360002
023700 R010-READ.                                                       02370002
023800     READ TRANS-FILE AT END MOVE 'Y' TO TRANS-EOF                 02380003
023900     END-READ                                                     02390003
024000     IF TRANS-PRESENT                                             02400003
024100       PERFORM P100-VALIDATE                                      02410003
024200     END-IF                                                       02420003
024300     IF TRANS-PRESENT                                             02430004
024400       PERFORM P600-PRINT-TRAN                                    02440005
024500     END-IF                                                       02450004
024600     IF TRANS-PRESENT AND (VALID-TRANS OR TRAN-WARN)              02460032
024700       PERFORM P300-PROCESS                                       02470003
024800     END-IF                                                       02480004
024900     IF TRANS-PRESENT                                             02490004
025000       PERFORM P500-REPORT                                        02500005
025100     END-IF                                                       02510004
025200     IF TRANS-PRESENT AND (TRAN-WARN OR TRAN-ERR)                 02520032
025300       PERFORM P800-ERR-RPT                                       02530005
025400     END-IF                                                       02540004
025500     EXIT.                                                        02550004
025600                                                                  02560004
025700 P100-VALIDATE.                                                   02570004
025800     MOVE SPACE TO TRANS-STATUS                                   02580017
025900     IF TI-TRAN = 'A '                                            02590005
026000       IF TB-DESCR = SPACE                                        02600005
026100         MOVE 'E'  TO TRANS-STATUS                                02610005
026200         MOVE ALL '*' TO P-DESCR OF ERR-DTL                       02620010
026300       END-IF                                                     02630005
026400       IF TB-PO-QTY NOT NUMERIC                                   02640005
026500         MOVE 'E' TO TRANS-STATUS                                 02650005
026600         MOVE ALL '*' TO P-ORD-QTY-X OF ERR-DTL                   02660005
026700         MOVE TB-PO-QTY TO P-ORD-QTY-X OF PRT-DTL                 02670005
026800       END-IF                                                     02680005
026900       IF TB-PO-AMT NOT NUMERIC                                   02690005
027000         MOVE 'E' TO TRANS-STATUS                                 02700005
027100         MOVE ALL '*' TO P-ORD-QTY-X OF ERR-DTL                   02710005
027200         MOVE TB-PO-AMT-X TO P-ORD-QTY-X OF PRT-DTL               02720015
027300       END-IF                                                     02730005
027400       IF NOT (TB-PRTY = '1' OR '2' OR '3')                       02740005
027500         MOVE 'E' TO TRANS-STATUS                                 02750005
027600         MOVE '*' TO P-PRTY-CD OF ERR-DTL                         02760005
027700         MOVE TB-PRTY TO P-PRTY-CD OF PRT-DTL                     02770005
027800       END-IF                                                     02780005
027900     END-IF                                                       02790005
028000     IF TI-TRAN = 'UB'                                            02800005
028100       IF TB-PO-QTY-X NOT = SPACE                                 02810015
028200         IF TB-PO-QTY NOT NUMERIC                                 02820005
028300           MOVE 'E' TO TRANS-STATUS                               02830005
028400           MOVE ALL '*' TO P-ORD-QTY-X OF ERR-DTL                 02840005
028500           MOVE TB-PO-QTY-X TO P-ORD-QTY-X OF PRT-DTL             02850015
028600         END-IF                                                   02860005
028700       END-IF                                                     02870005
028800       IF TB-PO-AMT-X NOT = SPACE                                 02880015
028900         IF TB-PO-AMT NOT NUMERIC                                 02890005
029000           MOVE 'E' TO TRANS-STATUS                               02900005
029100           MOVE ALL '*' TO P-ORD-AMT-X OF ERR-DTL                 02910005
029200           MOVE TB-PO-AMT-X TO P-ORD-AMT-X OF PRT-DTL             02920015
029300         END-IF                                                   02930005
029400       END-IF                                                     02940005
029500       IF TB-PRTY NOT = SPACE                                     02950005
029600         IF NOT (TB-PRTY = '1' OR '2' OR '3')                     02960005
029700           MOVE 'E' TO TRANS-STATUS                               02970005
029800           MOVE '*' TO P-PRTY-CD OF ERR-DTL                       02980005
029900           MOVE TB-PRTY TO P-PRTY-CD OF PRT-DTL                   02990005
030000         END-IF                                                   03000005
030100       END-IF                                                     03010005
030200     END-IF                                                       03020005
030300     IF TI-TRAN = 'UA'                                            03030005
030400       IF TA-UNITS-STARTED-X NOT = SPACE                          03040015
030500         IF TA-UNITS-STARTED NOT NUMERIC                          03050010
030600           MOVE 'E' TO TRANS-STATUS                               03060005
030700           MOVE ALL '*' TO P-UNIT-STRT-X OF ERR-DTL               03070005
030800           MOVE TA-UNITS-STARTED-X TO P-UNIT-STRT-X OF PRT-DTL    03080015
030900         END-IF                                                   03090019
031000       END-IF                                                     03100019
031100       IF TA-UNITS-COMPL-X NOT = SPACE                            03110015
031200         IF TA-UNITS-COMPL NOT NUMERIC                            03120012
031300           MOVE 'E' TO TRANS-STATUS                               03130005
031400           MOVE ALL '*' TO P-UNIT-COMP-X OF ERR-DTL               03140013
031500           MOVE TA-UNITS-COMPL-X TO P-UNIT-COMP-X OF PRT-DTL      03150015
031600         END-IF                                                   03160019
031700       END-IF                                                     03170019
031800     END-IF                                                       03180005
031900     MOVE 'V' TO TRANS-STATUS                                     03190017
032000     EXIT.                                                        03200004
032100 P300-PROCESS.                                                    03210010
032200     MOVE SPACE TO PRT-DTL                                        03220005
032300     MOVE TI-ORDR-NO TO P-ORD-NUM OF PRT-DTL                      03230010
032400     IF TI-TRAN = 'UB'                                            03240032
032500       MOVE 'UPD B' TO P-TRANS OF PRT-DTL                         03250032
032600     ELSE IF TI-TRAN = 'UA'                                       03260032
032700         MOVE 'UPD A' TO P-TRANS OF PRT-DTL                       03270032
032800       ELSE IF TI-TRAN = 'D '                                     03280032
032900           MOVE 'DEL  ' TO P-TRANS OF PRT-DTL                     03290032
033000         ELSE IF TI-TRAN = 'Q '                                   03300032
033100             MOVE 'QUERY' TO P-TRANS OF PRT-DTL                   03310032
033200           END-IF                                                 03320032
033300         END-IF                                                   03330032
033400       END-IF                                                     03340032
033500     END-IF                                                       03350032
033600*    NEED TO READ THE EXISTING SEGMENT                            03360005
033700     IF TI-TRAN = 'UA' OR 'UB' OR 'D ' OR 'Q '                    03370011
033800       MOVE 'GU  ' TO ACTION                                      03380012
033900       IF TI-TRAN NOT = 'Q '                                      03390012
034000         MOVE 'GHU ' TO ACTION                                    03400005
034100       END-IF                                                     03410005
034200       MOVE TI-ORDR-NO TO SSA-1-KEY                               03420005
034300       CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA, SSA-1 03430013
034400       DISPLAY 'DBPCB: ' DBPCB                                    03440036
034500*      MOVE 'PORDRA  ' TO AIBRSNM1                                03450033
034600*      MOVE 167 TO AIBOALEN                                       03460033
034700*      CALL 'AIBTDLI' USING ACTION, AIB, ORDER-ROOT-DATA, SSA-1   03470033
034800*      SET ADDRESS OF DBPCB TO AIBPTR                             03480033
034900       IF DBSTATUS NOT = '  '                                     03490005
035000         IF DBSTATUS = 'GE'                                       03500005
035100           MOVE 'NOT FOUND' TO P-ACT OF PRT-DTL                   03510013
035200           MOVE 'NOT FOUND' TO P-ACT OF ERR-DTL                   03520032
035300         ELSE                                                     03530005
035400           MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                  03540013
035500           MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                  03550032
035600           PERFORM P900-IMS-ERR                                   03560025
035700         END-IF                                                   03570005
035800       MOVE 'E' TO TRANS-STATUS                                   03580005
035900       END-IF                                                     03590005
036000     END-IF                                                       03600005
036100     IF NOT TRAN-ERR                                              03610005
036200       MOVE 'V' TO TRANS-STATUS                                   03620005
036300       IF TI-TRAN = 'D '                                          03630011
036400         MOVE 'DLET' TO ACTION                                    03640032
036500         CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA      03650032
036600         DISPLAY 'DBPCB: ' DBPCB                                  03660036
036700*        MOVE 'PORDRA  ' TO AIBRSNM1                              03670033
036800*        MOVE 167 TO AIBOALEN                                     03680033
036900*        CALL 'AIBTDLI' USING ACTION, AIB, ORDER-ROOT-DATA        03690033
037000*        SET ADDRESS OF DBPCB TO AIBPTR                           03700033
037100*        MOVE 'PORDRA  ' TO AIBRSNM1                              03710033
037200*        MOVE 167 TO AIBOALEN                                     03720033
037300*        CALL 'AIBTDLI' USING ACTION, AIB, ORDER-ROOT-DATA        03730033
037400*        SET ADDRESS OF DBPCB TO AIBPTR                           03740033
037500         IF DBSTATUS NOT = '  '                                   03750005
037600           MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                  03760005
037700           MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                  03770032
037800           PERFORM P900-IMS-ERR                                   03780025
037900         ELSE                                                     03790005
038000           MOVE 'DELETED' TO P-ACT OF PRT-DTL                     03800005
038100         END-IF                                                   03810005
038200         PERFORM P500-REPORT                                      03820010
038300       END-IF                                                     03830005
038400       IF TI-TRAN = 'A '                                          03840011
038500*        MOVE, MOVE, MOVE...                                      03850014
038600         MOVE SPACES TO ORDER-ROOT-DATA                           03860032
038700         MOVE 0 TO ACTUAL-ORDER-QUANTITY                          03870032
038800         MOVE 0 TO TOTAL-SCRAP-QUANTITY                           03880032
038900         MOVE 0 TO ORDER-STATUS                                   03890032
039000         MOVE 0 TO NUMBER-UNITS-STARTED(1)                        03900032
039100         MOVE 0 TO NUMBER-UNITS-COMPLETED(1)                      03910032
039200         MOVE 0 TO PERCENTAGE-STARTED(1)                          03920032
039300         MOVE 0 TO PERCENTAGE-COMPLETE(1)                         03930032
039400         MOVE 0 TO NUMBER-UNITS-STARTED(2)                        03940032
039500         MOVE 0 TO NUMBER-UNITS-COMPLETED(2)                      03950032
039600         MOVE 0 TO PERCENTAGE-STARTED(2)                          03960032
039700         MOVE 0 TO PERCENTAGE-COMPLETE(2)                         03970032
039800         MOVE 0 TO NUMBER-UNITS-STARTED(3)                        03980032
039900         MOVE 0 TO NUMBER-UNITS-COMPLETED(3)                      03990032
040000         MOVE 0 TO PERCENTAGE-STARTED(3)                          04000032
040100         MOVE 0 TO PERCENTAGE-COMPLETE(3)                         04010032
040200         MOVE 0 TO NUMBER-UNITS-STARTED(4)                        04020032
040300         MOVE 0 TO NUMBER-UNITS-COMPLETED(4)                      04030032
040400         MOVE 0 TO PERCENTAGE-STARTED(4)                          04040032
040500         MOVE 0 TO PERCENTAGE-COMPLETE(4)                         04050032
040600         MOVE 0 TO NUMBER-UNITS-STARTED(5)                        04060032
040700         MOVE 0 TO NUMBER-UNITS-COMPLETED(5)                      04070032
040800         MOVE 0 TO PERCENTAGE-STARTED(5)                          04080032
040900         MOVE 0 TO PERCENTAGE-COMPLETE(5)                         04090032
041000         MOVE TI-ORDR-NO TO ORDER-ROOT-KEY                        04100032
041100         MOVE TI-ORDR-NO TO SSA-1-KEY                             04110032
041200         MOVE TB-CUST-NO TO CUSTOMER-NUMBER                       04120032
041300         MOVE TB-DESCR  TO ORDER-DESCRIPTION                      04130032
041400         MOVE TB-PO-QTY TO PLANNED-ORDER-QUANTITY                 04140032
041500         MOVE TB-PO-AMT TO PLANNED-ORDER-AMOUNT                   04150032
041600         MOVE TB-ORDR-TYPE TO ORDER-TYPE                          04160032
041700         MOVE TB-PRTY TO PRIORITY-CODE                            04170032
041800                                                                  04180005
041900         MOVE 'ISRT' TO ACTION                                    04190005
042000         CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA,     04200013
042100             SSA-2                                                04210032
042200         DISPLAY 'DBPCB: ' DBPCB                                  04220036
042300*        MOVE 'PORDRA  ' TO AIBRSNM1                              04230033
042400*        MOVE 167 TO AIBOALEN                                     04240033
042500*        CALL 'AIBTDLI' USING ACTION, AIB, ORDER-ROOT-DATA, SSA-2 04250033
042600*        SET ADDRESS OF DBPCB TO AIBPTR                           04260033
042700         IF DBSTATUS NOT = SPACE                                  04270005
042800           MOVE 'E' TO TRANS-STATUS                               04280014
042900           IF DBSTATUS = 'II'                                     04290005
043000             MOVE 'DUPLICATE' TO P-ACT OF PRT-DTL                 04300005
043100             MOVE 'DUPLICATE' TO P-ACT OF ERR-DTL                 04310032
043200           ELSE                                                   04320005
043300             MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                04330032
043400             MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                04340032
043500             PERFORM P900-IMS-ERR                                 04350032
043600           END-IF                                                 04360005
043700         ELSE                                                     04370032
043800             MOVE 'ADDED    ' TO P-ACT OF PRT-DTL                 04380032
043900         END-IF                                                   04390005
044000         PERFORM P500-REPORT                                      04400010
044100       END-IF                                                     04410005
044200       IF TI-TRAN = 'UB'                                          04420014
044300*         MOVE, MOVE, MOVE...                                     04430014
044400         IF TB-CUST-NO NOT = SPACES                               04440032
044500           MOVE TB-CUST-NO TO CUSTOMER-NUMBER                     04450032
044600         END-IF                                                   04460032
044700         IF TB-DESCR NOT = SPACES                                 04470032
044800           MOVE TB-DESCR  TO ORDER-DESCRIPTION                    04480032
044900         END-IF                                                   04490032
045000         IF TB-PO-QTY-X NOT = SPACES                              04500032
045100           MOVE TB-PO-QTY-X TO PLANNED-ORDER-QUANTITY             04510032
045200         END-IF                                                   04520032
045300         IF TB-PO-AMT-X NOT = SPACES                              04530032
045400           MOVE TB-PO-AMT TO PLANNED-ORDER-AMOUNT                 04540032
045500         END-IF                                                   04550032
045600         IF TB-ORDR-TYPE NOT = SPACE                              04560032
045700           MOVE TB-ORDR-TYPE TO ORDER-TYPE                        04570032
045800         END-IF                                                   04580032
045900         IF TB-PRTY NOT = SPACE                                   04590032
046000           MOVE TB-PRTY TO PRIORITY-CODE                          04600032
046100         END-IF                                                   04610032
046200                                                                  04620005
046300         MOVE 'REPL' TO ACTION                                    04630005
046400         CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA      04640033
046500         DISPLAY 'DBPCB: ' DBPCB                                  04650036
046600*                                                                 04660033
046700*        MOVE 'PORDRA  ' TO AIBRSNM1                              04670033
046800*        MOVE 167 TO AIBOALEN                                     04680033
046900*        CALL 'AIBTDLI' USING ACTION, AIB, ORDER-ROOT-DATA        04690033
047000*        SET ADDRESS OF DBPCB TO AIBPTR                           04700033
047100         IF DBSTATUS NOT = SPACE                                  04710005
047200           MOVE 'E' TO TRANS-STATUS                               04720014
047300           MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                  04730005
047400           MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                  04740032
047500           PERFORM P900-IMS-ERR                                   04750025
047600         ELSE                                                     04760005
047700           MOVE 'CHANGED' TO P-ACT OF PRT-DTL                     04770005
047800         END-IF                                                   04780005
047900         PERFORM P500-REPORT                                      04790010
048000       END-IF                                                     04800005
048100       IF TI-TRAN = 'UA'                                          04810014
048200*         MOVE, MOVE, MOVE...                                     04820014
048300         IF TA-ORD-STATUS NUMERIC                                 04830032
048400           MOVE TA-ORD-STATUS TO ORDER-STATUS                     04840032
048500         END-IF                                                   04850032
048600         IF TA-ACT-DATE NOT = SPACE                               04860032
048700           MOVE TA-ACT-DATE TO FIRST-ACTIVITY-DATE                04870032
048800         END-IF                                                   04880032
048900         IF TA-LAST-ACT-DATE NOT = SPACE                          04890032
049000           MOVE TA-LAST-ACT-DATE TO LAST-ACTIVITY-DATE            04900032
049100         END-IF                                                   04910032
049200*         NOT TOTALLY SURE HOW THIS SHOULD WORK, BUT              04920032
049300*          LOOK FOR OPEN SLOT TO ADD STARTED/COMPL                04930032
049400                                                                  04940005
049500         MOVE 'REPL' TO ACTION                                    04950005
049600         CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA      04960032
049700         DISPLAY 'DBPCB: ' DBPCB                                  04970036
049800*                                                                 04980033
049900*        MOVE 'PORDRA  ' TO AIBRSNM1                              04990033
050000*        MOVE 167 TO AIBOALEN                                     05000033
050100*        CALL 'AIBTDLI' USING ACTION, AIB, ORDER-ROOT-DATA        05010033
050200*        SET ADDRESS OF DBPCB TO AIBPTR                           05020033
050300         IF DBSTATUS NOT = SPACE                                  05030005
050400           MOVE 'E' TO TRANS-STATUS                               05040014
050500           MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                  05050005
050600           MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                  05060032
050700           PERFORM P900-IMS-ERR                                   05070025
050800         ELSE                                                     05080005
050900           MOVE 'CHANGED' TO P-ACT OF PRT-DTL                     05090005
051000         END-IF                                                   05100005
051100         PERFORM P500-REPORT                                      05110010
051200       END-IF                                                     05120005
051300     END-IF                                                       05130005
051400     EXIT.                                                        05140005
051500 P500-REPORT.                                                     05150010
051600     IF (CUR-LINE-CNT + 5) > MAX-LINES                            05160010
051700       MOVE H1 TO RPT-REC                                         05170010
051800       PERFORM P750-PRINT                                         05180019
051900       MOVE H2 TO RPT-REC                                         05190010
052000       PERFORM P750-PRINT                                         05200019
052100       MOVE '0' TO P-CC OF PRT-DTL                                05210015
052200       MOVE 3 TO CUR-LINE-CNT                                     05220010
052300     END-IF                                                       05230010
052400     IF (ORDER-ROOT-KEY = SSA-1-KEY)                              05240029
052500       MOVE 1 TO LOGICAL-LINE                                     05250029
052600       MOVE ORDER-ROOT-KEY TO         P-ORD-NUM OF PRT-DTL        05260029
052700       MOVE ORDER-DESCRIPTION TO      P-DESCR OF PRT-DTL          05270029
052800       MOVE CUSTOMER-NUMBER TO        P-CUST-NO OF PRT-DTL        05280029
052900       MOVE PLANNED-ORDER-QUANTITY TO P-ORD-QTY OF PRT-DTL        05290029
053000       MOVE PLANNED-ORDER-AMOUNT   TO P-ORD-AMT OF PRT-DTL        05300032
053100       MOVE ORDER-TYPE TO             P-TYPE OF PRT-DTL           05310029
053200       MOVE ACTUAL-ORDER-QUANTITY TO  P-ACT-ORD-QTY OF PRT-DTL    05320029
053300*      MOVE TOTAL-SCRAP-QUANTITY TO   P-TOT-SCRAP OF PRT-DTL      05330030
053400       MOVE ORDER-STATUS TO           P-ORD-STAT OF PRT-DTL       05340029
053500       MOVE PRIORITY-CODE TO          P-PRTY-CD OF PRT-DTL        05350029
053600       MOVE FIRST-ACTIVITY-DATE TO W-DATE                         05360032
053700         MOVE 'YYYY/MM/DD' TO W-DATE-FMT                          05370032
053800         MOVE CORRESPONDING W-DATE TO W-DATE-FMT                  05380032
053900         MOVE W-DATE-FMT TO P-DATE OF PRT-DTL                     05390032
054000       MOVE LAST-ACTIVITY-DATE TO W-DATE                          05400032
054100         MOVE 'YYYY/MM/DD' TO W-DATE-FMT                          05410032
054200         MOVE CORRESPONDING W-DATE TO W-DATE-FMT                  05420032
054300         MOVE W-DATE-FMT TO P-DATE-L OF PRT-DTL                   05430032
054400       PERFORM P700-PRINT                                         05440029
054500       PERFORM VARYING LOGICAL-LINE FROM 1 BY 1                   05450029
054600           UNTIL LOGICAL-LINE > 5                                 05460029
054700         IF NUMBER-UNITS-STARTED (LOGICAL-LINE) > 0               05470029
054800           MOVE SPACES TO PRT-DTL                                 05480029
054800           MOVE SPACES TO ERR-DTL                                 05481039
054900           MOVE 'STARTED' TO P-STRT-CAPT OF PRT-DTL               05490029
055000           MOVE NUMBER-UNITS-STARTED(LOGICAL-LINE) TO             05500029
055100                P-UNIT-STRT OF PRT-DTL                            05510029
055200           MOVE PERCENTAGE-STARTED (LOGICAL-LINE) TO              05520029
055300                P-PCT-STRT OF PRT-DTL                             05530029
055400           MOVE NUMBER-UNITS-COMPLETED(LOGICAL-LINE) TO           05540029
055500                P-PCT-COMP OF PRT-DTL                             05550029
055600           MOVE 'COMPL' TO P-COMP-CAPT OF PRT-DTL                 05560029
055700           PERFORM  P700-PRINT                                    05570032
055800         END-IF                                                   05580029
055900       END-PERFORM                                                05590029
056000     ELSE                                                         05600029
056100       PERFORM P700-PRINT                                         05610029
056200     END-IF                                                       05620029
056300     MOVE SPACES TO PRT-DTL                                       05630015
056300     MOVE SPACES TO ERR-DTL                                       05631039
056400     EXIT.                                                        05640010
056500 P600-PRINT-TRAN.                                                 05650009
056600     IF (CUR-LINE-CNT + 1) > MAX-LINES                            05660009
056700       MOVE H1 TO RPT-REC                                         05670009
056800       PERFORM P750-PRINT                                         05680019
056900       MOVE H2 TO RPT-REC                                         05690009
057000       PERFORM P750-PRINT                                         05700019
057100       MOVE '0' TO P-CC OF PRT-DTL                                05710015
057200       MOVE 3 TO CUR-LINE-CNT                                     05720009
057300     END-IF                                                       05730009
057400     MOVE SPACE TO PRT-DTL                                        05740020
057500     MOVE TI-TRAN TO P-TRANS OF PRT-DTL                           05750021
057600     MOVE TI-ORDR-NO TO P-ORD-NUM OF PRT-DTL                      05760021
057700     IF TI-TRAN = 'A ' OR TI-TRAN = 'UB'                          05770032
057800       MOVE TB-CUST-NO TO P-CUST-NO OF PRT-DTL                    05780032
057900       MOVE TB-DESCR TO P-DESCR OF PRT-DTL                        05790032
058000       MOVE TB-PO-QTY-X TO P-ORD-QTY-X OF PRT-DTL                 05800032
058100       MOVE TB-PO-AMT-X TO P-ORD-AMT-X OF PRT-DTL                 05810032
058200       MOVE TB-ORDR-TYPE TO P-TYPE OF PRT-DTL                     05820032
058300       MOVE TB-PRTY TO P-PRTY-CD OF PRT-DTL                       05830032
058400     END-IF                                                       05840032
058500     IF TI-TRAN = 'UA'                                            05850032
058600       MOVE TA-ORD-STATUS TO P-ORD-STAT-X OF PRT-DTL              05860032
058700       MOVE TA-ACT-DATE TO W-DATE                                 05870032
058800         MOVE 'YYYY/MM/DD' TO W-DATE-FMT                          05880032
058900         MOVE CORRESPONDING W-DATE TO W-DATE-FMT                  05890032
059000         MOVE W-DATE-FMT TO P-DATE OF PRT-DTL                     05900032
059100       MOVE TA-LAST-ACT-DATE TO W-DATE                            05910032
059200         MOVE 'YYYY/MM/DD' TO W-DATE-FMT                          05920032
059300         MOVE CORRESPONDING W-DATE TO W-DATE-FMT                  05930032
059400         MOVE W-DATE-FMT TO P-DATE-L OF PRT-DTL                   05940032
059500       MOVE TA-UNITS-STARTED-X TO P-UNIT-STRT-X OF PRT-DTL        05950032
059600       MOVE TA-UNITS-COMPL-X TO P-UNIT-COMP-X OF PRT-DTL          05960032
059700     END-IF                                                       05970032
059800     MOVE PRT-DTL TO RPT-REC                                      05980032
059900     PERFORM P750-PRINT                                           05990018
060000     MOVE SPACES TO PRT-DTL                                       06000015
060000     MOVE SPACES TO ERR-DTL                                       06001039
060100     EXIT.                                                        06010009
060200 P700-PRINT.                                                      06020018
060300     MOVE PRT-DTL TO RPT-REC                                      06030015
060400     PERFORM P750-PRINT                                           06040018
060500     EXIT.                                                        06050005
060600 P750-PRINT.                                                      06060018
060700     MOVE 1 TO ADV-LINES                                          06070018
060800     IF R-CC = '0'                                                06080018
060900       MOVE 2 TO ADV-LINES                                        06090018
061000     END-IF                                                       06100018
061100     IF R-CC = 'T'                                                06110018
061200       MOVE 0 TO ADV-LINES                                        06120018
061300     END-IF                                                       06130018
061400     IF R-CC = '-'                                                06140018
061500       MOVE 3 TO ADV-LINES                                        06150018
061600     END-IF                                                       06160018
061700     WRITE RPT-REC AFTER ADVANCING ADV-LINES                      06170018
061800     ADD ADV-LINES TO CUR-LINE-CNT                                06180018
061900     EXIT.                                                        06190018
062000 P800-ERR-RPT.                                                    06200005
062100     IF (ERR-LINE-CNT + 5) > MAX-LINES                            06210029
062200       MOVE H1-E TO ERR-REC                                       06220032
062300       PERFORM P850-ERR-PRINT                                     06230029
062400       MOVE H2 TO ERR-REC                                         06240032
062500       PERFORM P850-ERR-PRINT                                     06250029
062600       MOVE '0' TO P-CC OF PRT-DTL                                06260029
062700       MOVE 3 TO ERR-LINE-CNT                                     06270032
062800     END-IF                                                       06280029
062900     MOVE TI-TRAN TO P-TRANS OF ERR-DTL                           06290032
063000     IF (ORDER-ROOT-KEY = SSA-1-KEY)                              06300029
063100       MOVE 1 TO LOGICAL-LINE                                     06310029
063200       MOVE ORDER-ROOT-KEY TO         P-ORD-NUM OF ERR-DTL        06320029
063300       MOVE ORDER-DESCRIPTION TO      P-DESCR OF ERR-DTL          06330029
063400       MOVE CUSTOMER-NUMBER TO        P-CUST-NO OF ERR-DTL        06340029
063500       MOVE PLANNED-ORDER-QUANTITY TO P-ORD-QTY OF ERR-DTL        06350029
063600       MOVE PLANNED-ORDER-AMOUNT TO   P-ORD-AMT OF ERR-DTL        06360032
063700       MOVE ORDER-TYPE TO             P-TYPE OF ERR-DTL           06370029
063800       MOVE ACTUAL-ORDER-QUANTITY TO  P-ACT-ORD-QTY OF ERR-DTL    06380029
063900*      MOVE TOTAL-SCRAP-QUANTITY TO   P-TOT-SCRAP OF ERR-DTL      06390029
064000       MOVE ORDER-STATUS TO           P-ORD-STAT OF ERR-DTL       06400029
064100       MOVE PRIORITY-CODE TO          P-PRTY-CD OF ERR-DTL        06410029
064200     END-IF                                                       06420029
064300     PERFORM P810-ERR-PRINT                                       06430032
064400     EXIT.                                                        06440029
064500 P810-ERR-PRINT.                                                  06450029
064600     MOVE ERR-DTL TO ERR-REC                                      06460029
064700     PERFORM P850-ERR-PRINT                                       06470029
064800     MOVE SPACES TO ERR-DTL                                       06480032
064900     EXIT.                                                        06490029
065000 P850-ERR-PRINT.                                                  06500029
065100     MOVE 1 TO ADV-LINES                                          06510005
065200     IF E-CC = '0'                                                06520005
065300       MOVE 2 TO ADV-LINES                                        06530005
065400     END-IF                                                       06540005
065500     IF E-CC = 'T'                                                06550005
065600       MOVE 0 TO ADV-LINES                                        06560005
065700     END-IF                                                       06570005
065800     IF E-CC = '-'                                                06580005
065900       MOVE 3 TO ADV-LINES                                        06590005
066000     END-IF                                                       06600005
066100     WRITE ERR-REC AFTER ADVANCING ADV-LINES                      06610005
066200     ADD ADV-LINES TO ERR-LINE-CNT                                06620032
066300     EXIT.                                                        06630005
066400 P900-IMS-ERR.                                                    06640025
066500     DISPLAY 'IMS ERROR DBD: ' DBD-NAME 'DBSTATUS: ' DBSTATUS     06650025
066600     DISPLAY '   SEG-LEVEL: ' SEG-LEVEL ' PROCOPTIONS: '          06660025
066700        PROC-OPTIONS                                              06670025
066800     DISPLAY '   SEG-NAME-FB: '                                   06680026
066900        SEG-NAME-FB                                               06690025
067000     DISPLAY '   LENGTH-FB-KEY: ' LENGTH-FB-KEY ' NUM SENS SEGS: '06700026
067100        NUMB-SENS-SEGS                                            06710026
067200     DISPLAY '   KEY-FB-AREA: ' KEY-FB-AREA                       06720026
067300     DISPLAY '   SSA: ' SSA-1                                     06730025
067400     EXIT.                                                        06740025