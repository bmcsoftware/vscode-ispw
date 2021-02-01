//#USERIDX JOB (MMC),'C/370 SAMPLE',CLASS=L,
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
//*   EXECUTE CWXTMAIN IN BATCH
//*
//CWXTMAIN EXEC PGM=CWXTMAIN,PARM=00003
//*
//STEPLIB  DD  DISP=SHR,DSN=SALESSUP.#APL.#LVL.LOAD
//*
//EMPFILE  DD  DISP=SHR,DSN=SYS2.CW.&CWGAXT..SLXTSAMP(CWXTDATA)
//RPTFILE  DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//