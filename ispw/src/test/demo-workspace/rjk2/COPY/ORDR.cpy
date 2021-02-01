000100 01  ORDER-ROOT-DATA.                                             00010000
000200     05 ORDER-ROOT-KEY.                                           00020000
000300         07 ORDER-NUMBER-PREFIX      PIC  X(02).                  00030000
000400         07 ORDER-NUMBER             PIC  9(04).                  00040000
000500     05  ORDKEY-REDEF  REDEFINES    ORDER-ROOT-KEY                00050000
000600                                     PIC  X(06).                  00060001
000700     05 ORDER-DESCRIPTION            PIC  X(40).                  00070001
000800     05 CUSTOMER-NUMBER              PIC  X(06).                  00080001
000900     05 PLANNED-ORDER-QUANTITY       PIC  S9(05)       COMP-3.    00090001
001000     05 PLANNED-ORDER-AMOUNT         PIC  9(05)V99     COMP-3.    00100001
001100     05 ORDER-TYPE                   PIC  X(02).                  00110001
001200     05 ACTUAL-ORDER-QUANTITY        PIC  S9(05)       COMP-3.    00120001
001300     05 TOTAL-SCRAP-QUANTITY         PIC  S9(05)       COMP-3.    00130001
001400     05 TOTAL-SCRAP-REDEFINES                                     00140002
001500         REDEFINES TOTAL-SCRAP-QUANTITY PIC  X(03).               00150002
001600     05 ORDER-STATUS                 PIC  9(02).                  00160001
001700     05 FILLER                       PIC  X(01).                  00170001
001800     05 FIRST-ACTIVITY-DATE.                                      00180000
001900         07 FIRST-ACTIVITY-DATE-YR   PIC  X(04).                  00190000
002000         07 FIRST-ACTIVITY-DATE-MM   PIC  X(02).                  00200000
002100         07 FIRST-ACTIVITY-DATE-DD   PIC  X(02).                  00210000
002200     05 LAST-ACTIVITY-DATE           PIC  X(08).                  00220001
002300     05 WEEKLY-STATUS-DATA OCCURS       5 TIMES.                  00230003
002400         07 NUMBER-UNITS-STARTED     PIC  9(05)        COMP-3.    00240000
002500         07 NUMBER-UNITS-COMPLETED   PIC  9(05)        COMP-3.    00250000
002600         07 PERCENTAGE-STARTED       PIC  S9(05)V9(03) COMP-3.    00260000
002700         07 PERCENTAGE-COMPLETE      PIC  S9(05)V9(03) COMP-3.    00270000
002800     05 PRIORITY-CODE                PIC  X(01).                  00280001