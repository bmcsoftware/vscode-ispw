      ******************************************************
      *   COPY BOOK TCPYB04
      *   THIS IS A TRAINING COPY BOOK - FOR LINKAGE SECTION
      ******************************************************
       01  PASS-ME-AROUND.
           05  INFILE-EOF-FLAG        PIC X(1).
               88  INFILE-EOF             VALUE 'Y'.
           05  ACTION-FLAG            PIC X(1).
               88  ACTION-READ        VALUE 'R'.
               88  ACTION-CLOSE       VALUE 'C'.
           05  INCOMING-DATA.
               10  KEEPER-TAG         PIC X(6).
                  88  KEEPER          VALUE '<KEEP>'.
               10  TEXT-PORTION       PIC X(74).