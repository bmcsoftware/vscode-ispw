 CBL  APOST                                                             00010000
       IDENTIFICATION DIVISION.                                         00020000
       PROGRAM-ID.  DFSIVP34.                                           00030000
      *                                                                 00040000
      ********************************************************@SCPYRT** 00050000
      *                                                               * 00060000
      *  Licensed Materials - Property of IBM                         * 00070000
      *                                                               * 00080000
      *  5635-A06                                                     * 00090000
      *                                                               * 00100000
      *      Copyright IBM Corp. 1991,1998 All Rights Reserved        * 00110000
      *                                                               * 00120000
      *  US Government Users Restricted Rights - Use, duplication or  * 00130000
      *  disclosure restricted by GSA ADP Schedule contract with      * 00140000
      *  IBM Corp.                                                    * 00150000
      *                                                               * 00160000
      ********************************************************@ECPYRT** 00170000
      *                                                                 00180000
      *   APPLICATION  :  CONVERSATIONAL PROGRAM                        00190000
      *   TRANSACTION  :  IVTCB                                         00200000
      *   PSB          :  DFSIVP34                                      00210000
      *   DATABASE     :  DFSIVD2                                       00220000
      *   INPUT:                                                        00230000
      *         TELEPHONE DIRECTORY SYSTEM                              00240000
      *         PROCESS CODE : CCCCCCCC                                 00250000
      *         LAST NAME    : XXXXXXXXXX                               00260000
      *         FIRST NAME   : XXXXXXXXXX                               00270000
      *         EXTENSION#   : N-NNN-NNNN                               00280000
      *         INTERNAL ZIP : XXX/XXX                                  00290000
      *   CCCCCCCC = COMMAND                                            00300000
      *         ADD = INSERT ENTRY IN DB                                00310000
      *         DELETE = DELETE ENTRY FROM DB                           00320000
      *         UPDATE = UPDATE ENTRY FROM DB                           00330000
      *         DISPLAY = DISPLAY ENTRY                                 00340000
      *         TADD = SAME AS ADD, BUT ALSO WRITE TO OPERATOR          00350000
      *         END = TERMINATE CONVERSATION                            00360000
      *                                                                 00370000
      *       CHANGES:  THIS MODULE IS NEW IN IMS/ESA 3.2               00380000
      *  APAR...  ID  PREREQ.  DATE....  DESCRIPTION................... 00390000
      *  KNQ0115  01           11/17/91  ADD COBOL LANG VERSION         00400000
      *                                                                 00410000
                                                                        00420000
       ENVIRONMENT DIVISION.                                            00430000
       CONFIGURATION SECTION.                                           00440000
       SOURCE-COMPUTER. IBM-370.                                        00450000
       OBJECT-COMPUTER. IBM-370.                                        00460000
      *                                                                 00470000
       DATA DIVISION.                                                   00480000
       WORKING-STORAGE SECTION.                                         00490000
                                                                        00500000
      * DL/I FUNCTION CODES                                             00510000
                                                                        00520000
       77  GET-UNIQUE       PICTURE X(4)  VALUE 'GU  '.                 00530000
       77  GET-HOLD-UNIQUE  PICTURE X(4)  VALUE 'GHU '.                 00540000
       77  GET-NEXT         PICTURE X(4)  VALUE 'GN  '.                 00550000
       77  GET-HOLD-NEXT    PICTURE X(4)  VALUE 'GHN '.                 00560000
       77  DLET             PICTURE X(4)  VALUE 'DLET'.                 00570000
       77  ISRT             PICTURE X(4)  VALUE 'ISRT'.                 00580000
       77  REPL             PICTURE X(4)  VALUE 'REPL'.                 00590000
                                                                        00600000
      * DL/I CALL STATUS CODES                                          00610000
                                                                        00620000
       77  MESSAGE-EXIST    PIC X(2) VALUE 'CF'.                        00630000
       77  NO-MORE-SEGMENT  PIC X(2) VALUE 'QD'.                        00640000
       77  NO-MORE-MESSAGE  PIC X(2) VALUE 'QC'.                        00650000
                                                                        00660000
      * MESSAGES                                                        00670000
                                                                        00680000
       77  MDEL    PICTURE X(40) VALUE                                  00690000
                     'ENTRY WAS DELETED                       '.        00700000
       77  MADD    PICTURE X(40) VALUE                                  00710000
                     'ENTRY WAS ADDED                         '.        00720000
       77  MDIS    PICTURE X(40) VALUE                                  00730000
                     'ENTRY WAS DISPLAYED                     '.        00740000
       77  MUPD    PICTURE X(40) VALUE                                  00750000
                     'ENTRY WAS UPDATED                       '.        00760000
       77  MEND    PICTURE X(40) VALUE                                  00770000
                     'CONVERSATION HAS ENDED                  '.        00780000
       77  MMORE   PICTURE X(40) VALUE                                  00790000
                     'DATA IS NOT ENOUGH.  PLEASE KEY IN MORE '.        00800000
       77  MINV    PICTURE X(40) VALUE                                  00810000
                     'PROCESS CODE IS NOT VALID               '.        00820000
       77  MNODATA PICTURE X(40) VALUE                                  00830000
                     'NO DATA WAS INPUT.  PLEASE KEY IN MORE  '.        00840000
       77  MNONAME PICTURE X(40) VALUE                                  00850000
                     'LAST NAME WAS NOT SPECIFIED             '.        00860000
       77  MNOENT  PICTURE X(40) VALUE                                  00870000
                     'SPECIFIED PERSON WAS NOT FOUND          '.        00880000
       77  MISRTE  PICTURE X(40) VALUE                                  00890000
                     'ADDITION OF ENTRY HAS FAILED            '.        00900000
       77  MDLETE  PICTURE X(40) VALUE                                  00910000
                     'DELETION OF ENTRY HAS FAILED            '.        00920000
       77  MREPLE  PICTURE X(40) VALUE                                  00930000
                     'UPDATE OF ENTRY HAS FAILED              '.        00940000
                                                                        00950000
      * VARIABLES                                                       00960000
                                                                        00970000
       77  SSA1    PICTURE X(9) VALUE 'A1111111 '.                      00980000
       77  MODNAME PICTURE X(8) VALUE SPACES.                           00990000
       77  TRAN-CODE  PICTURE X(8) VALUE 'IVTCB'.                       01000000
       77  REPLY      PICTURE X(16).                                    01010000
       77  TEMP-ONE   PICTURE X(8) VALUE SPACES.                        01020000
       77  TEMP-TWO   PICTURE X(8) VALUE SPACES.                        01030000
                                                                        01040000
      * DATA AREA FOR TERMINAL INPUT                                    01050000
                                                                        01060000
       01  INPUT-MSG.                                                   01070000
           02  IN-LL          PICTURE S9(3) COMP.                       01080000
           02  IN-ZZ          PICTURE S9(3) COMP.                       01090000
           02  IN-FILL        PICTURE X(4).                             01100000
           02  IN-COMMAND     PICTURE X(8).                             01110000
           02  TEMP-COMMAND REDEFINES IN-COMMAND.                       01120000
               04  TEMP-IOCMD    PIC X(3).                              01130000
               04  TEMP-FILLER   PIC X(5).                              01140000
           02  IN-LAST-NAME   PICTURE X(10).                            01150000
           02  IN-FIRST-NAME  PICTURE X(10).                            01160000
           02  IN-EXTENSION   PICTURE X(10).                            01170000
           02  IN-ZIP-CODE    PICTURE X(7).                             01180000
                                                                        01190000
      * DATA AREA OUTPUT                                                01200000
                                                                        01210000
       01  OUTPUT-AREA.                                                 01220000
           02  OUT-LL       PICTURE S9(3) COMP VALUE +95.               01230000
           02  OUT-ZZ       PICTURE S9(3) COMP VALUE +0.                01240000
           02  OUTPUT-LINE  PICTURE X(85) VALUE SPACES.                 01250000
           02  OUTPUT-DATA REDEFINES OUTPUT-LINE.                       01260000
               04  OUT-MESSAGE   PIC X(40).                             01270000
               04  OUT-COMMAND   PIC X(8).                              01280000
               04  OUT-DATA-TYPE.                                       01290000
                   06  OUT-LAST-NAME   PIC X(10).                       01300000
                   06  OUT-FIRST-NAME  PIC X(10).                       01310000
                   06  OUT-EXTENSION   PIC X(10).                       01320000
                   06  OUT-ZIP-CODE    PIC X(7).                        01330000
           02  OUT-SEGMENT-NO  PICTURE X(4) VALUE '0001'.               01340000
                                                                        01350000
      * I/O AREA FOR DATA BASE HANDLING                                 01360000
                                                                        01370000
       01  IOAREA.                                                      01380000
           02  IO-LINE PICTURE X(37) VALUE SPACES.                      01390000
           02  IO-DATA REDEFINES IO-LINE.                               01400000
               04  IO-LAST-NAME    PIC X(10).                           01410000
               04  IO-FIRST-NAME   PIC X(10).                           01420000
               04  IO-EXTENSION    PIC X(10).                           01430000
               04  IO-ZIP-CODE     PIC X(7).                            01440000
           02  IO-FILLER       PIC X(3) VALUE SPACES.                   01450000
           02  IO-COMMAND      PIC X(8) VALUE SPACES.                   01460000
                                                                        01470000
      * SCRATCH PAD AREA                                                01480000
                                                                        01490000
       01  SPA.                                                         01500000
           02  SPA-LL        PICTURE X(2).                              01510000
           02  SPA-ZZ        PICTURE X(4).                              01520000
           02  SPA-TRANCODE  PICTURE X(8).                              01530000
           02  SPA-CALL      PICTURE X(2).                              01540000
           02  SPA-COMMAND   PICTURE X(8).                              01550000
           02  SPA-DATA.                                                01560000
               04  SPA-LAST-NAME    PIC X(10).                          01570000
               04  SPA-FIRST-NAME   PIC X(10).                          01580000
               04  SPA-EXTENSION    PIC X(10).                          01590000
               04  SPA-ZIP-CODE     PIC X(7).                           01600000
           02  FILLER        PICTURE X(19).                             01610000
                                                                        01620000
      * DC TEXT FOR ERROR CALL                                          01630000
                                                                        01640000
       01 DC-TEXT.                                                      01650000
          02  TEXT1         PIC  X(7) VALUE 'STATUS '.                  01660000
          02  ERROR-STATUS  PIC  X(2).                                  01670000
          02  TEXT2         PIC  X(12) VALUE 'DLI  CALL = '.            01680000
          02  ERROR-CALL    PIC  X(4).                                  01690000
                                                                        01700000
      * SEGMENT SEARCH ARGUMENT                                         01710000
                                                                        01720000
       01 SSA.                                                          01730000
          02  SEGMENT-NAME  PIC X(8)  VALUE 'A1111111'.                 01740000
          02  SEG-KEY-NAME  PIC X(11) VALUE '(A1111111 ='.              01750000
          02  SSA-KEY       PIC X(10).                                  01760000
          02  FILLER        PIC X VALUE ')'.                            01770000
                                                                        01780000
      * FLAGS                                                           01790000
                                                                        01800000
       01 FLAGS.                                                        01810000
          02  SET-DATA-FLAG  PIC X VALUE '0'.                           01820000
             88  NO-SET-DATA       VALUE '1'.                           01830000
          02  TADD-FLAG      PIC X VALUE '0'.                           01840000
             88  PROCESS-TADD      VALUE '1'.                           01850000
                                                                        01860000
      * COUNTERS                                                        01870000
                                                                        01880000
       01 COUNTERS.                                                     01890000
          02  SPA-CALL-NO    PIC   9(2) COMP VALUE 0.                   01900000
          02  L-SPACE-CTR    PIC   9(2) COMP VALUE 0.                   01910000
                                                                        01920000
       LINKAGE SECTION.                                                 01930000
                                                                        01940000
       01  IOPCB.                                                       01950000
           02  LTERM-NAME   PICTURE X(8).                               01960000
           02  FILLER       PICTURE X(2).                               01970000
           02  TPSTATUS     PICTURE XX.                                 01980000
           02  FILLER       PICTURE X(20).                              01990000
       01  DBPCB.                                                       02000000
           02  DBNAME       PICTURE X(8).                               02010000
           02  SEG-LEVEL-NO PICTURE X(2).                               02020000
           02  DBSTATUS     PICTURE XX.                                 02030000
           02  FILLER       PICTURE X(20).                              02040000
                                                                        02050000
       PROCEDURE DIVISION USING IOPCB, DBPCB.                           02060000
                                                                        02070000
      * ON ENTRY IMS PASSES ADDRESSES FOR IOPCB AND DBPCB               02080000
                                                                        02090000
       MAIN-RTN.                                                        02100000
           MOVE GET-UNIQUE TO ERROR-CALL.                               02110000
           CALL 'CBLTDLI' USING GET-UNIQUE, IOPCB, SPA.                 02120000
           IF TPSTATUS  = '  ' OR MESSAGE-EXIST                         02130000
           THEN                                                         02140000
             CALL 'CBLTDLI' USING GET-NEXT, IOPCB, INPUT-MSG            02150000
             IF TPSTATUS = SPACES                                       02160000
               THEN PERFORM PROCESS-INPUT THRU PROCESS-INPUT-END        02170000
             ELSE IF TPSTATUS = NO-MORE-SEGMENT                         02180000
                  THEN GOBACK                                           02190000
                  ELSE                                                  02200000
                    MOVE GET-NEXT TO ERROR-CALL                         02210000
                    PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END        02220000
           ELSE IF TPSTATUS = NO-MORE-MESSAGE                           02230000
                THEN GOBACK                                             02240000
                ELSE PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.      02250000
           GOBACK.                                                      02260000
                                                                        02270000
      * PROCEDURE PROCESS-INPUT                                         02280000
                                                                        02290000
       PROCESS-INPUT.                                                   02300000
           IF IN-LL < 5                                                 02310000
             MOVE MNODATA TO OUT-MESSAGE                                02320000
             PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                02330000
                                                                        02340000
      *    CHECK THE LEADING SPACE IN INPUT COMMAND AND TRIM IT OFF     02350000
                                                                        02360000
           INSPECT IN-COMMAND TALLYING L-SPACE-CTR FOR LEADING SPACE    02370000
             REPLACING LEADING SPACE BY '*'.                            02380000
           IF L-SPACE-CTR > 0                                           02390000
             UNSTRING IN-COMMAND DELIMITED BY ALL '*' INTO TEMP-ONE     02400000
               TEMP-TWO                                                 02410000
             MOVE TEMP-TWO TO IN-COMMAND                                02420000
             MOVE 0 TO L-SPACE-CTR                                      02430000
             MOVE SPACES TO TEMP-TWO.                                   02440000
                                                                        02450000
      *    CHECK THE LEADING SPACE IN INPUT LAST NAME AND TRIM IT OFF   02460000
                                                                        02470000
           INSPECT IN-LAST-NAME TALLYING L-SPACE-CTR FOR LEADING        02480000
             SPACE REPLACING LEADING SPACE BY '*'.                      02490000
           IF L-SPACE-CTR > 0                                           02500000
             UNSTRING IN-LAST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE   02510000
               TEMP-TWO                                                 02520000
             MOVE TEMP-TWO TO IN-LAST-NAME                              02530000
             MOVE 0 TO L-SPACE-CTR                                      02540000
             MOVE SPACES TO TEMP-TWO.                                   02550000
                                                                        02560000
      *    CHECK THE LEADING SPACE IN INPUT FIRST NAME AND TRIM IT OFF  02570000
                                                                        02580000
           INSPECT IN-FIRST-NAME TALLYING L-SPACE-CTR FOR LEADING       02590000
             SPACE REPLACING LEADING SPACE BY '*'.                      02600000
           IF L-SPACE-CTR > 0                                           02610000
             UNSTRING IN-FIRST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE  02620000
               TEMP-TWO                                                 02630000
             MOVE TEMP-TWO TO IN-FIRST-NAME                             02640000
             MOVE 0 TO L-SPACE-CTR                                      02650000
             MOVE SPACES TO TEMP-TWO.                                   02660000
                                                                        02670000
      *    CHECK THE LEADING SPACE IN INPUT EXTENSION AND TRIM IT OFF   02680000
                                                                        02690000
           INSPECT IN-EXTENSION TALLYING L-SPACE-CTR FOR LEADING        02700000
             SPACE REPLACING LEADING SPACE BY '*'.                      02710000
           IF L-SPACE-CTR > 0                                           02720000
             UNSTRING IN-EXTENSION DELIMITED BY ALL '*' INTO TEMP-ONE   02730000
               TEMP-TWO                                                 02740000
             MOVE TEMP-TWO TO IN-EXTENSION                              02750000
             MOVE 0 TO L-SPACE-CTR                                      02760000
             MOVE SPACES TO TEMP-TWO.                                   02770000
                                                                        02780000
      *    CHECK THE LEADING SPACE IN INPUT ZIP CODE AND TRIM IT OFF    02790000
                                                                        02800000
           INSPECT IN-ZIP-CODE TALLYING L-SPACE-CTR FOR LEADING SPACE   02810000
             REPLACING LEADING SPACE BY '*'.                            02820000
           IF L-SPACE-CTR > 0                                           02830000
             UNSTRING IN-ZIP-CODE DELIMITED BY ALL '*' INTO TEMP-ONE    02840000
               TEMP-TWO                                                 02850000
             MOVE TEMP-TWO TO IN-ZIP-CODE                               02860000
             MOVE 0 TO L-SPACE-CTR                                      02870000
             MOVE SPACES TO TEMP-TWO.                                   02880000
      *                                                                 02890000
           MOVE IN-LAST-NAME TO IO-LAST-NAME.                           02900000
           MOVE IN-COMMAND TO IO-COMMAND.                               02910000
           IF SPA-CALL-NO = 0                                           02920000
             MOVE IN-LAST-NAME TO IO-LAST-NAME                          02930000
             MOVE IN-COMMAND TO IO-COMMAND                              02940000
           ELSE IF IN-LAST-NAME EQUAL SPACES                            02950000
                THEN MOVE SPA-LAST-NAME TO IO-LAST-NAME                 02960000
                ELSE MOVE IN-LAST-NAME TO IO-LAST-NAME.                 02970000
                                                                        02980000
           IF IN-COMMAND EQUAL SPACES                                   02990000
             MOVE SPA-COMMAND TO IO-COMMAND                             03000000
           ELSE                                                         03010000
             MOVE IN-COMMAND TO IO-COMMAND.                             03020000
                                                                        03030000
           IF IO-COMMAND EQUAL SPACES                                   03040000
           THEN MOVE MINV TO OUT-MESSAGE                                03050000
                PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END              03060000
           ELSE IF IO-LAST-NAME EQUAL SPACES AND TEMP-IOCMD NOT = 'END' 03070000
                THEN MOVE MNONAME TO OUT-MESSAGE                        03080000
                    PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END          03090000
           ELSE IF TEMP-IOCMD EQUAL 'ADD'                               03100000
                THEN PERFORM TO-ADD THRU TO-ADD-END                     03110000
           ELSE IF TEMP-IOCMD EQUAL 'TAD'                               03120000
                THEN MOVE 1 TO TADD-FLAG                                03130000
                    PERFORM TO-ADD THRU TO-ADD-END                      03140000
           ELSE IF TEMP-IOCMD EQUAL 'UPD'                               03150000
                THEN PERFORM TO-UPD THRU TO-UPD-END                     03160000
           ELSE IF TEMP-IOCMD EQUAL 'DEL'                               03170000
                THEN PERFORM TO-DEL THRU TO-DEL-END                     03180000
           ELSE IF TEMP-IOCMD EQUAL 'DIS'                               03190000
                THEN PERFORM TO-DIS THRU TO-DIS-END                     03200000
           ELSE IF TEMP-IOCMD EQUAL 'END'                               03210000
                THEN PERFORM TO-END THRU TO-END-END                     03220000
           ELSE                                                         03230000
               MOVE MINV TO OUT-MESSAGE                                 03240000
               PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.              03250000
       PROCESS-INPUT-END.                                               03260000
           EXIT.                                                        03270000
                                                                        03280000
      * PROCEDURE TO-ADD : ADDITION REQUEST HANDLER                     03290000
                                                                        03300000
       TO-ADD.                                                          03310000
           IF IO-LAST-NAME EQUAL SPA-LAST-NAME                          03320000
           THEN MOVE SPA-DATA TO IO-DATA.                               03330000
           IF IN-FIRST-NAME EQUAL SPACES OR                             03340000
              IN-EXTENSION EQUAL SPACES OR                              03350000
              IN-ZIP-CODE EQUAL SPACES                                  03360000
           THEN                                                         03370000
              MOVE MMORE TO OUT-MESSAGE                                 03380000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                03390000
           ELSE                                                         03400000
              MOVE IN-FIRST-NAME TO IO-FIRST-NAME                       03410000
              MOVE IN-EXTENSION  TO IO-EXTENSION                        03420000
              MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                         03430000
              MOVE IO-DATA       TO SPA-DATA                            03440000
              MOVE IO-DATA       TO OUT-DATA-TYPE                       03450000
              MOVE IO-COMMAND    TO OUT-COMMAND                         03460000
              PERFORM ISRT-DB THRU ISRT-DB-END.                         03470000
       TO-ADD-END.                                                      03480000
           EXIT.                                                        03490000
                                                                        03500000
      * PROCEDURE TO-UPD : UPDATE REQUEST HANDLER                       03510000
                                                                        03520000
       TO-UPD.                                                          03530000
           MOVE 0 TO SET-DATA-FLAG.                                     03540000
           MOVE IO-LAST-NAME TO SSA-KEY.                                03550000
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.      03560000
           IF DBSTATUS = SPACES                                         03570000
           THEN                                                         03580000
             IF IN-FIRST-NAME NOT = SPACES                              03590000
               MOVE 1 TO SET-DATA-FLAG                                  03600000
               MOVE IN-FIRST-NAME TO IO-FIRST-NAME                      03610000
             END-IF                                                     03620000
             IF IN-EXTENSION  NOT = SPACES                              03630000
               MOVE 1 TO SET-DATA-FLAG                                  03640000
               MOVE IN-EXTENSION  TO IO-EXTENSION                       03650000
             END-IF                                                     03660000
             IF IN-ZIP-CODE   NOT = SPACES                              03670000
               MOVE 1 TO SET-DATA-FLAG                                  03680000
               MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                        03690000
             END-IF                                                     03700000
             MOVE IO-DATA TO OUT-DATA-TYPE.                             03710000
             MOVE IO-COMMAND TO OUT-COMMAND.                            03720000
             IF NO-SET-DATA                                             03730000
             THEN                                                       03740000
               PERFORM REPL-DB THRU REPL-DB-END                         03750000
             ELSE                                                       03760000
               MOVE MNODATA TO OUT-MESSAGE                              03770000
               PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.              03780000
       TO-UPD-END.                                                      03790000
           EXIT.                                                        03800000
                                                                        03810000
      * PROCEDURE TO-DEL : DELETE REQUEST HANDLER                       03820000
                                                                        03830000
       TO-DEL.                                                          03840000
           MOVE IO-LAST-NAME TO SSA-KEY.                                03850000
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.      03860000
           IF DBSTATUS = SPACES                                         03870000
           THEN                                                         03880000
              MOVE IO-DATA TO OUT-DATA-TYPE                             03890000
              MOVE IO-COMMAND TO OUT-COMMAND                            03900000
              PERFORM DLET-DB THRU DLET-DB-END.                         03910000
       TO-DEL-END.                                                      03920000
           EXIT.                                                        03930000
                                                                        03940000
      * PROCEDURE TO-DIS : DISPLAY REQUEST HANDLER                      03950000
                                                                        03960000
       TO-DIS.                                                          03970000
           MOVE IO-LAST-NAME TO SSA-KEY.                                03980000
           PERFORM GET-UNIQUE-DB THRU GET-UNIQUE-DB-END.                03990000
           IF DBSTATUS = SPACES                                         04000000
           THEN                                                         04010000
              MOVE IO-DATA TO OUT-DATA-TYPE                             04020000
              MOVE IO-COMMAND TO OUT-COMMAND                            04030000
              MOVE MDIS TO OUT-MESSAGE                                  04040000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04050000
       TO-DIS-END.                                                      04060000
           EXIT.                                                        04070000
                                                                        04080000
      * PROCEDURE TO-END : END REQUEST HANDLER                          04090000
                                                                        04100000
       TO-END.                                                          04110000
           MOVE SPACES TO SPA-TRANCODE.                                 04120000
           MOVE MEND TO OUT-MESSAGE.                                    04130000
           PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                  04140000
       TO-END-END.                                                      04150000
           EXIT.                                                        04160000
                                                                        04170000
      * PROCEDURE ISRT-DB : DATA BASE SEGMENT INSERT REQUEST HANDLER    04180000
                                                                        04190000
       ISRT-DB.                                                         04200000
           MOVE ISRT TO ERROR-CALL.                                     04210000
           CALL 'CBLTDLI' USING ISRT, DBPCB, IOAREA, SSA1.              04220000
           IF DBSTATUS  = SPACES                                        04230000
           THEN                                                         04240000
              IF PROCESS-TADD                                           04250000
                 DISPLAY 'INSERT IS DONE, REPLY' UPON CONSOLE           04260000
                 ACCEPT REPLY FROM CONSOLE                              04270000
                 MOVE 0 TO TADD-FLAG                                    04280000
              END-IF                                                    04290000
              MOVE MADD TO OUT-MESSAGE                                  04300000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                04310000
           ELSE                                                         04320000
              MOVE MISRTE TO OUT-MESSAGE                                04330000
              MOVE DBSTATUS TO ERROR-STATUS                             04340000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04350000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04360000
       ISRT-DB-END.                                                     04370000
           EXIT.                                                        04380000
                                                                        04390000
      * PROCEDURE GET-UNIQUE-DB                                         04400000
      *    DATA BASE SEGMENT GET-UNIQUE-DB REQUEST HANDLER              04410000
                                                                        04420000
       GET-UNIQUE-DB.                                                   04430000
           MOVE GET-UNIQUE TO ERROR-CALL.                               04440000
           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB, IOAREA, SSA.         04450000
           IF DBSTATUS NOT = SPACES                                     04460000
           THEN                                                         04470000
              MOVE MNOENT TO OUT-MESSAGE                                04480000
              MOVE DBSTATUS TO ERROR-STATUS                             04490000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04500000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04510000
       GET-UNIQUE-DB-END.                                               04520000
           EXIT.                                                        04530000
                                                                        04540000
      * PROCEDURE GET-HOLD-UNIQUE-DB                                    04550000
      *    DATA BASE SEGMENT GET-HOLD-UNIQUE-DB REQUEST HANDLER         04560000
                                                                        04570000
       GET-HOLD-UNIQUE-DB.                                              04580000
           MOVE GET-HOLD-UNIQUE TO ERROR-CALL.                          04590000
           CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DBPCB, IOAREA, SSA.    04600000
           IF DBSTATUS NOT = SPACES                                     04610000
           THEN                                                         04620000
              MOVE MNOENT TO OUT-MESSAGE                                04630000
              MOVE DBSTATUS TO ERROR-STATUS                             04640000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04650000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04660000
       GET-HOLD-UNIQUE-DB-END.                                          04670000
           EXIT.                                                        04680000
                                                                        04690000
      * PROCEDURE REPL-DB : DATA BASE SEGMENT REPLACE REQUEST HANDLER   04700000
                                                                        04710000
       REPL-DB.                                                         04720000
           MOVE REPL TO ERROR-CALL.                                     04730000
           CALL 'CBLTDLI' USING REPL, DBPCB, IOAREA.                    04740000
           IF DBSTATUS = SPACES                                         04750000
           THEN                                                         04760000
              MOVE MUPD TO OUT-MESSAGE                                  04770000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                04780000
           ELSE                                                         04790000
              MOVE MREPLE TO OUT-MESSAGE                                04800000
              MOVE DBSTATUS TO ERROR-STATUS                             04810000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04820000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04830000
       REPL-DB-END.                                                     04840000
           EXIT.                                                        04850000
                                                                        04860000
      * PROCEDURE DLET-DB : DATA BASE SEGMENT DELETE REQUEST HANDLER    04870000
                                                                        04880000
       DLET-DB.                                                         04890000
           MOVE DLET TO ERROR-CALL.                                     04900000
           CALL 'CBLTDLI' USING DLET, DBPCB, IOAREA.                    04910000
           IF DBSTATUS = SPACES                                         04920000
           THEN                                                         04930000
              MOVE MDEL TO OUT-MESSAGE                                  04940000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                04950000
           ELSE                                                         04960000
              MOVE MDLETE TO OUT-MESSAGE                                04970000
              MOVE DBSTATUS TO ERROR-STATUS                             04980000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04990000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               05000000
       DLET-DB-END.                                                     05010000
           EXIT.                                                        05020000
                                                                        05030000
      * PROCEDURE TERM-ROUTINE : TERMINAL ROUTINE                       05040000
                                                                        05050000
       TERM-ROUTINE.                                                    05060000
           MOVE SPACES TO MODNAME.                                      05070000
           PERFORM INSERT-SPA THRU INSERT-SPA-END.                      05080000
           IF IN-COMMAND = 'END'                                        05090000
             MOVE TRAN-CODE TO MODNAME.                                 05100000
           PERFORM INSERT-IO THRU INSERT-IO-END.                        05110000
       TERM-ROUTINE-END.                                                05120000
           EXIT.                                                        05130000
                                                                        05140000
      * PROCEDURE INSERT-SPA : SPA INSERT FOR IOPCB REQUEST HANDLER     05150000
                                                                        05160000
       INSERT-SPA.                                                      05170000
           MOVE ISRT TO ERROR-CALL.                                     05180000
           MOVE IO-DATA TO SPA-DATA.                                    05190000
           MOVE IO-COMMAND TO SPA-COMMAND.                              05200000
           ADD 1 TO SPA-CALL-NO.                                        05210000
           MOVE SPA-CALL-NO TO SPA-CALL.                                05220000
           CALL 'CBLTDLI' USING ISRT, IOPCB, SPA.                       05230000
           IF TPSTATUS NOT = SPACES                                     05240000
             THEN PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.         05250000
       INSERT-SPA-END.                                                  05260000
           EXIT.                                                        05270000
                                                                        05280000
      * PROCEDURE INSERT-IO : INSERT FOR IOPCB REQUEST HANDLER          05290000
                                                                        05300000
       INSERT-IO.                                                       05310000
           MOVE ISRT TO ERROR-CALL.                                     05320000
           IF MODNAME EQUAL SPACES                                      05330000
           THEN                                                         05340000
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA             05350000
           ELSE                                                         05360000
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA, MODNAME    05370000
              MOVE SPACES TO MODNAME.                                   05380000
           IF TPSTATUS NOT = SPACES                                     05390000
             THEN PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.         05400000
       INSERT-IO-END.                                                   05410000
           EXIT.                                                        05420000
                                                                        05430000
      * PROCEDURE WRITE-DC-TEXT : WRITE ERROR STATUS CODE               05440000
                                                                        05450000
       WRITE-DC-TEXT.                                                   05460000
           MOVE TPSTATUS TO ERROR-STATUS.                               05470000
           DISPLAY DC-TEXT UPON CONSOLE.                                05480000
       WRITE-DC-TEXT-END.                                               05490000
           EXIT.                                                        05500000
                                                                        05510000
                                                                        05520000