*>*****************************************************************
      *> FILE:    WS-CONSTANTS.cpy
      *> PURPOSE: Application-wide named constants. All magic numbers
      *>          are defined here so they can be updated in one place.
      *>
      *> CONSTANTS:
      *>   WS-CONST-MAX-ACCOUNTS     - Max accounts in memory (5)
      *>   WS-CONST-MAX-PROFILES     - Max profiles in memory (5)
      *>   WS-CONST-MAX-PENDING      - Max pending requests in memory (50)
      *>   WS-CONST-MAX-CONNECTIONS  - Max connections in memory (50)
      *>   WS-CONST-MAX-JOBS         - Max job postings in memory (25)
      *>   WS-CONST-MAX-EXPERIENCES  - Max experience entries per profile (3)
      *>   WS-CONST-MAX-EDUCATIONS   - Max education entries per profile (3)
      *>   WS-CONST-MAX-APPLICATIONS - Max applications in memory (25)
      *>   WS-CONST-FS-OK            - File status "00" (success)
      *>   WS-CONST-FS-NOT-FOUND     - File status "35" (file not found)
      *>   WS-CONST-FS-OPEN-OK       - File status "97" (open success variant)
      *>   WS-CONST-YES / WS-CONST-NO       - "Y" / "N" boolean flags
      *>   WS-CONST-TRUE / WS-CONST-FALSE   - 1 / 0 numeric booleans
      *>   WS-CONST-PENDING-STATUS   - "P" pending request status sentinel
      *>
      *> USED BY: All procedure copybooks and main.cob
      *>*****************************************************************
      01  WS-CONST-MAX-ACCOUNTS          PIC 9     VALUE 5.
      01  WS-CONST-MAX-PROFILES          PIC 9     VALUE 5.
      01  WS-CONST-MAX-PENDING           PIC 99    VALUE 50.
      01  WS-CONST-MAX-CONNECTIONS       PIC 99    VALUE 50.
      01  WS-CONST-MAX-JOBS              PIC 999   VALUE 25.
      01  WS-CONST-MAX-EXPERIENCES       PIC 9     VALUE 3.
      01  WS-CONST-MAX-EDUCATIONS        PIC 9     VALUE 3.
      01  WS-CONST-MAX-APPLICATIONS      PIC 999   VALUE 25.

      *> File status codes
      01  WS-CONST-FS-OK                 PIC XX    VALUE "00".
      01  WS-CONST-FS-NOT-FOUND          PIC XX    VALUE "35".
      01  WS-CONST-FS-OPEN-OK            PIC XX    VALUE "97".

      *> Boolean flag values
      01  WS-CONST-YES                   PIC X     VALUE "Y".
      01  WS-CONST-NO                    PIC X     VALUE "N".
      01  WS-CONST-TRUE                  PIC 9     VALUE 1.
      01  WS-CONST-FALSE                 PIC 9     VALUE 0.

      *> Status sentinel values
      01  WS-CONST-PENDING-STATUS        PIC X     VALUE "P".
