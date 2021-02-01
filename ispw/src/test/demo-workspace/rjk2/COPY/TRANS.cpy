       01  TRAN-INQY.
           03  TI-TRAN             PIC XX.
      *        A  - ADD ORDER
      *        UB - UPDATE BASE INFO
      *        UA - UPDATE ACTIVITY
      *        D  - DELETE ORDER
      *        Q  - QUERY
           03  TI-ORDR-NO          PIC X(6).
           03  FILLER              PIC X(72).
       01  TRAN-ORD-BASE.
           03  FILLER              PIC X(8).
           03  TB-CUST-NO          PIC X(6).
           03  TB-DESCR            PIC X(40).
           03  TB-PO-QTY           PIC 9(5).
           03  TB-PO-QTY-X REDEFINES TB-PO-QTY PIC X(5).
           03  TB-PO-AMT           PIC 9(5)V99.
           03  TB-PO-AMT-X REDEFINES TB-PO-AMT PIC X(7).
           03  TB-ORDR-TYPE        PIC XX.
           03  TB-PRTY             PIC X.
           03  FILLER              PIC X(11).
       01  TRAN-ACTIVITY.
           03  FILLER              PIC X(8).
           03  TA-ORD-STATUS       PIC 99.
           03  TA-ACT-DATE         PIC X(8).
           03  TA-LAST-ACT-DATE    PIC X(8).
           03  TA-UNITS-STARTED    PIC 9(5).
           03  TA-UNITS-STARTED-X REDEFINES TA-UNITS-STARTED PIC X(5).
           03  TA-UNITS-COMPL      PIC 9(5).
           03  TA-UNITS-COMPL-X REDEFINES TA-UNITS-COMPL PIC X(5).
           03  FILLER              PIC X(44).