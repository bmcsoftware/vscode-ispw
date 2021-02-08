//HCHRJK0A JOB ('#SALESSUP'),'CWXTJCLC', TYPRUN=SCAN,
//             CLASS=A,MSGCLASS=R,NOTIFY=&SYSUID
/*JOBPARM S=*
//*
//*********************************************************************
//*                                                                   *
//*  MEMBER = CWXTJCLC                                                *
//*                                                                   *
//*  THIS IS SAMPLE JCL TO EXECUTE COBOL PROGRAM CWXTCOB              *
//*                                                                   *
//*********************************************************************
//*
//*   EXECUTE CWXTCOB IN BATCH
//*
//CWXTCOB  EXEC PGM=CWXTCOB,PARM=00001
//*
//STEPLIB  DD  DISP=SHR,DSN=SALESSUP.RJK1.DEV1.LOAD
//         DD  DISP=SHR,DSN=SALESSUP.RJK1.QA1.LOAD
//         DD  DISP=SHR,DSN=SALESSUP.RJK1.STG.LOAD
//         DD  DISP=SHR,DSN=SALESSUP.RJK1.PRD.LOAD
//*
//EMPFILE  DD  DISP=SHR,DSN=SALESSUP.RJK1.PRD.CWXTDATA
//RPTFILE  DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*