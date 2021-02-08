//HCHRJK0A JOB ('#SALESSUP'),'DFSMPR01',TYPRUN=SCAN,
//             CLASS=A,MSGCLASS=R,NOTIFY=&SYSUID
/*JOBPARM S=*
//DFSMPR01       PROC SOUT=A,RGN=4M,
//            CL1=001,CL2=000,CL3=000,CL4=000,                          00000020
//            OPT=N,OVLA=0,SPIE=0,VALCK=0,TLIM=00,                      00000030
//            PCB=000,PRLD=,STIMER=,SOD=,DBLDL=,                        00000040
//            NBA=,OBA=,IMSID=IDCC,AGN=,VSFX=,VFREE=,                   00000050
//            SSM=EXSS,PREINIT=,ALTID=,PWFI=N,                          00000060
//            APARM=,LOCKMAX=,APPLFE=,                                  00000070
//            ENVIRON=,JVMOPMAS=                                        00000080
//*                                                                     00000090
//REGION EXEC PGM=DFSRRC00,REGION=&RGN,                                 00000100
//            TIME=1440,DPRTY=(12,0),                                   00000110
//            PARM=(MSG,&CL1&CL2&CL3&CL4,                               00000120
//            &OPT&OVLA&SPIE&VALCK&TLIM&PCB,                            00000130
//            &PRLD,&STIMER,&SOD,&DBLDL,&NBA,                           00000140
//            &OBA,&IMSID,&AGN,&VSFX,&VFREE,                            00000150
//            &SSM,&PREINIT,&ALTID,&PWFI,                               00000160
//            '&APARM',&LOCKMAX,&APPLFE,                                00000170
//            &ENVIRON,&JVMOPMAS)                                       00000180
//*                                                                     00000190
//STEPLIB  DD DISP=SHR,DSN=SALESSUP.RJK1.DEV1.LOAD                      00000200
//         DD DISP=SHR,DSN=SALESSUP.RJK1.DEV2.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.DEV3.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.EMR.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.FIX.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.QA1.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.QA2.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.QA3.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.STG.LOAD
//         DD DISP=SHR,DSN=SALESSUP.RJK1.PRD.LOAD
//         DD DSN=IMS131A.CWCC.PGMLIB,DISP=SHR                          00000200
//         DD DSN=IMS131A.CWCC.SDFSRESL,DISP=SHR                        00000210
//         DD DSN=IMS131A.SDFSRESL,DISP=SHR                             00000210
//* **     DD DISP=SHR,DSN=IX.IMS131A.CWCC.PGMLIB                       00000210
//* **     DD DISP=SHR,DSN=SALESSUP.KT.IMS131A.PGMLIB                   00000210
//         DD DISP=SHR,DSN=SYS2.CW.VJ.R1702.IMSDCWCC.PGMLIB             00000210
//         DD DISP=SHR,DSN=SYS2.CW.XT.R1702.MLXT170.PDSE.PGMLIB         00000210
//         DD DISP=SHR,DSN=SYS2.CW.XT.R1702.MLXT170.PGMLIB              00000210
//PROCLIB  DD DSN=IMS131A.CWCC.PROCLIB,DISP=SHR                         00000220
//SYSUDUMP DD SYSOUT=&SOUT,                                             00000230
//         DCB=(LRECL=121,BLKSIZE=3129,RECFM=VBA),                      00000240
//         SPACE=(125,(2500,100),RLSE,,ROUND)                           00000250
// PEND
//STEP1 EXEC DFSMPR01