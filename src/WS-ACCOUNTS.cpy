*>*****************************************************************
      *> FILE:    WS-ACCOUNTS.cpy
      *> PURPOSE: In-memory user accounts table and authentication
      *>          working-storage variables. Populated from ACCOUNTS.DAT
      *>          at startup by DATALOAD.cpy.
      *>
      *> VARIABLES:
      *>   WS-USER-ACCOUNTS     - Table of up to 5 accounts (OCCURS 5)
      *>     WS-USERNAME(n)     - Username (PIC X(20))
      *>     WS-PASSWORD(n)     - Password (PIC X(12))
      *>   WS-ACCOUNT-COUNT     - Number of accounts currently loaded
      *>   WS-PROFILE-COUNT     - Number of profiles currently loaded
      *>   WS-CURRENT-USER-INDEX - Index of the logged-in user
      *>   WS-LOGIN-USERNAME    - Username entered during login attempt
      *>   WS-LOGIN-PASSWORD    - Password entered during login attempt
      *>   WS-LOGIN-SUCCESS     - 1 = success, 0 = failed
      *>   WS-ACCOUNT-INDEX     - General-purpose loop index into table
      *>   WS-PASSWORD-INPUT    - Raw password input buffer (PIC X(50))
      *>   WS-PASSWORD-LENGTH   - Computed length of trimmed password
      *>   WS-HAS-CAPITAL       - 1 if password has an uppercase letter
      *>   WS-HAS-DIGIT         - 1 if password has a digit
      *>   WS-HAS-SPECIAL       - 1 if password has a special character
      *>   WS-PASSWORD-VALID    - 1 if all password rules pass
      *>   WS-CHAR-INDEX        - Loop index for character scan
      *>   WS-CURRENT-CHAR      - Character being examined
      *>
      *> NOTE: OCCURS 5 is governed by WS-CONST-MAX-ACCOUNTS in
      *>       WS-CONSTANTS.cpy — both must be kept in sync.
      *>*****************************************************************
01  WS-USER-ACCOUNTS.
    05  WS-ACCOUNT OCCURS 5 TIMES.
        10  WS-USERNAME         PIC X(20).
        10  WS-PASSWORD         PIC X(12).

01  WS-ACCOUNT-COUNT            PIC 9 VALUE 0.
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
