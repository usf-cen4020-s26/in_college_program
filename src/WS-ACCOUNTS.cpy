      *> ============================================================
      *> WS-ACCOUNTS.cpy - Account table and authentication variables
      *> ============================================================
01  WS-USER-ACCOUNTS.
    05  WS-ACCOUNT OCCURS 5 TIMES.
        10  WS-USERNAME         PIC X(20).
        10  WS-PASSWORD         PIC X(12).

01  WS-ACCOUNT-COUNT            PIC 9 VALUE 0.
01  WS-MAX-ACCOUNTS             PIC 9 VALUE WS-CONST-MAX-ACCOUNTS.
01  WS-PROFILE-COUNT            PIC 9 VALUE 0.

01  WS-LOGIN-USERNAME           PIC X(20).
01  WS-LOGIN-PASSWORD           PIC X(12).
01  WS-LOGIN-SUCCESS            PIC 9 VALUE 0.
01  WS-ACCOUNT-INDEX            PIC 9.

01  WS-PASSWORD-INPUT           PIC X(50).
01  WS-PASSWORD-LENGTH          PIC 99.
01  WS-HAS-CAPITAL              PIC 9 VALUE 0.
01  WS-HAS-DIGIT                PIC 9 VALUE 0.
01  WS-HAS-SPECIAL              PIC 9 VALUE 0.
01  WS-PASSWORD-VALID           PIC 9 VALUE 0.
01  WS-CHAR-INDEX               PIC 99.
01  WS-CURRENT-CHAR             PIC X.
