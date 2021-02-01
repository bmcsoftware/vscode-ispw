//#USERIDX JOB (MMC),'ASM SAMPLE',CLASS=L,
//  MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=#USERID
//*
//*JOBPARM S=CWCC
//* *******************************************************************
//*
//*  INSTRUCTIONS:
//*  =============
//*
//*  1) C ALL #APL      TO YOUR APPLICATION CODE
//*  2) C ALL #LVL      TO THE LEVEL YOUR PROGRAM IS CURRENTLY AT
//*  3) C ALL #USERID   TO YOUR USERID
//*  4) SAVE
//*  5) SUBMIT
//*
//* *******************************************************************
//*
//*  RUN TRIMAIN IN BATCH WITHOUT XPEDITER/TSO
//*
//TESTPROC PROC PROG=TRIMAIN,INPUT=NULLFILE
//RUNPROG  EXEC PGM=&PROG
//STEPLIB  DD  DSN=SALESSUP.#APL.#LVL.LOAD
//*
//*   ALLOCATE ALL INPUT AND OUTPUT DDNAMES
//*
//INFILE   DD  DSN=&INPUT,DISP=SHR
//OUTFILE  DD  SYSOUT=(*),TERM=TS
//SYSOUT   DD  SYSOUT=(*),TERM=TS
//         PEND
//*
//MYTEST   EXEC TESTPROC
//INFILE   DD  *
345
789
563
234
345
445
555
336
555
123
124
//