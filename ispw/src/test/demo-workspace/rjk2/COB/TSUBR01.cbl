       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSUBR01.
       AUTHOR.        COMPUWARE ISPW TRAINING.
       DATE-WRITTEN.  JANUARY 24TH, 1996.
       DATE-COMPILED.

      ********************************************************
      * THIS PROGRAM IS A TRAINING PROGRAM
      *      CALLED FROM    TPROG01
      *      WITH COPYLIB   TCPYB01 (LINKAGE AREA)
      *
      * Test the Build command 2020-01-09
      *
      * ======================================================
      *  ISPW (TM)
      *  COPYRIGHT (C) 1986-2016 COMPUWARE CORPORATION.
      *  UNPUBLISHED RIGHTS RESERVED UNDER THE COPYRIGHT
      *  LAWS OF THE UNITED STATES.
      *
      * ======================================================
      *                   MODIFICATION  LOG
      *
      *  DD/MM/YY  PROGRAMMER  CHANGES
      *  ********  **********  *******
      *  96/01/24  ISPW        NEW PROGRAM
      * ======================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ********************************************************
      *  FILES USED:
      *   INPUT   JUST SOME DUMMY DATA TO READ IN
      ********************************************************
           SELECT INFILE  ASSIGN UT-S-INPUT
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD  INFILE
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           DATA RECORD IS INPUT-REC.

       01  INPUT-REC         PIC X(80).

       WORKING-STORAGE SECTION.

       01   INFILE-OPEN-FLAG     PIC X(1) VALUE 'N'.
            88 INFILE-OPEN       VALUE 'Y'.

       LINKAGE SECTION.
      ********************************************************
      **** A COPY MEMBER FOR THE LINKAGE AREA
      ********************************************************
       COPY TCPYB01.

       PROCEDURE DIVISION USING PASS-ME-AROUND.

       00000-MAIN-PROCEDURE.

           IF NOT INFILE-OPEN THEN
              MOVE 'Y' TO INFILE-OPEN-FLAG
              MOVE 'N' TO INFILE-EOF-FLAG
              OPEN INPUT INFILE.

           MOVE SPACES TO INCOMING-DATA.

           IF ACTION-READ THEN
              PERFORM GET-ME-A-RECORD THRU GET-ME-A-RECORD-X
              UNTIL INFILE-EOF OR KEEPER.

           IF ACTION-CLOSE THEN
              CLOSE INFILE.

           GOBACK.

       GET-ME-A-RECORD.
           READ INFILE AT END MOVE 'Y' TO INFILE-EOF-FLAG.
           IF NOT INFILE-EOF MOVE INPUT-REC TO INCOMING-DATA.
       GET-ME-A-RECORD-X.
           EXIT.