//HCHRJK0A JOB ('#SALESSUP'),'REXX CXFXIMPO', TYPRUN=SCAN,
//             CLASS=A,MSGCLASS=R,NOTIFY=&SYSUID
/*JOBPARM S=*
//*
//* EXECUTE CXFXIMPO
//*
//STEP1 EXEC PGM=IKJEFT01,
//             PARM=CXFXIMPO
//SYSTSIN  DD DUMMY
//SYSTSPRT DD SYSOUT=*
//SYSPROC  DD DISP=SHR,DSN=SALESSUP.RJK1.DEV1.CLST