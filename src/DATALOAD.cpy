      *> ============================================================
      *> DATALOAD_SRC.cpy - Startup data loading from all .DAT files
      *> Paragraphs: 1100-1162 (accounts/profiles), 9200-9275 (pending/connections/messages)
      *> ============================================================
       1100-LOAD-ACCOUNTS.
           OPEN INPUT ACCOUNTS-FILE.
           IF WS-ACCOUNTS-STATUS = WS-CONST-FS-OK OR WS-ACCOUNTS-STATUS = WS-CONST-FS-OPEN-OK
               PERFORM 1110-READ-ACCOUNT-LOOP
               CLOSE ACCOUNTS-FILE
           END-IF.

       1110-READ-ACCOUNT-LOOP.
           READ ACCOUNTS-FILE
               AT END
                   MOVE 1 TO WS-EOF-FLAG
               NOT AT END
                   IF WS-ACCOUNT-COUNT < WS-MAX-ACCOUNTS
                       ADD 1 TO WS-ACCOUNT-COUNT
                       MOVE ACCT-USERNAME TO
                           WS-USERNAME(WS-ACCOUNT-COUNT)
                       MOVE ACCT-PASSWORD TO
                           WS-PASSWORD(WS-ACCOUNT-COUNT)
                   END-IF
           END-READ.

           IF WS-EOF-FLAG = 0
               PERFORM 1110-READ-ACCOUNT-LOOP
           ELSE
               MOVE 0 TO WS-EOF-FLAG
           END-IF.

*> *      *>*****************************************************************
*> *      *> 1150-LOAD-PROFILES: Load all profiles from PROFILES.dat       *
*> *      *> USER STORY (Epic 2): Profile persistence                      *
*> *      *>*****************************************************************
       1150-LOAD-PROFILES.
           OPEN INPUT PROFILES-FILE.
           IF WS-PROFILES-STATUS = WS-CONST-FS-OK OR WS-PROFILES-STATUS = WS-CONST-FS-OPEN-OK
               IF WS-PROFILES-STATUS = WS-CONST-FS-OK
                   PERFORM 1160-READ-PROFILE-LOOP
               END-IF
               CLOSE PROFILES-FILE
           END-IF.

*> *

*> *      *>*****************************************************************
*> *      *> 9200-LOAD-PENDING-REQUESTS: Load pending requests at startup *
*> *      *>*****************************************************************
       9200-LOAD-PENDING-REQUESTS.
           MOVE 0 TO WS-PENDING-COUNT.
           MOVE 0 TO WS-PENDING-EOF.

           OPEN INPUT PENDING-FILE.

           EVALUATE WS-PENDING-STATUS
               WHEN WS-CONST-FS-OK
                   PERFORM 9210-READ-PENDING-LOOP
                   CLOSE PENDING-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   *> file not found: that's ok (no pending yet)
                   MOVE 0 TO WS-PENDING-COUNT
               WHEN OTHER
                   *> unexpected error, but don't crash program
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open PENDING.DAT. FILE STATUS = "
                    WS-PENDING-STATUS
                    DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                    END-STRING
               PERFORM 8000-WRITE-OUTPUT

                   MOVE 0 TO WS-PENDING-COUNT
           END-EVALUATE.

           *> (Optional debug) show how many loaded
           *> MOVE SPACES TO WS-OUTPUT-LINE
           *> STRING "DEBUG: Pending requests loaded = "
           *>     WS-PENDING-COUNT
           *>     DELIMITED BY SIZE
           *>    INTO WS-OUTPUT-LINE
           *> END-STRING
           *> PERFORM 8000-WRITE-OUTPUT.
           EXIT.
*>*****************************************************************
*> 9250-LOAD-CONNECTIONS: Load established connections at startup
*>*****************************************************************
       9250-LOAD-CONNECTIONS.
           MOVE 0 TO WS-CONNECTIONS-COUNT.
           MOVE "N" TO WS-CONNECTIONS-EOF.

           OPEN INPUT CONNECTIONS-FILE.

           EVALUATE WS-CONNECTIONS-STATUS
               WHEN WS-CONST-FS-OK
                   PERFORM 9260-READ-CONNECTIONS-LOOP
                   CLOSE CONNECTIONS-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   *> file not found: ok (no connections yet)
                   MOVE 0 TO WS-CONNECTIONS-COUNT
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open CONNECTIONS.DAT. FILE STATUS = "
                       WS-CONNECTIONS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 0 TO WS-CONNECTIONS-COUNT
           END-EVALUATE.
           EXIT.

*>*****************************************************************
*> 9260-READ-CONNECTIONS-LOOP: Read CONNECTIONS.DAT into WS table
*>*****************************************************************
       9260-READ-CONNECTIONS-LOOP.
           READ CONNECTIONS-FILE
               AT END
                   MOVE "Y" TO WS-CONNECTIONS-EOF
               NOT AT END
                   IF WS-CONNECTIONS-COUNT < WS-MAX-CONNECTIONS
                       ADD 1 TO WS-CONNECTIONS-COUNT
                       MOVE CONN-USER-A TO
                           WS-CONN-USER-A(WS-CONNECTIONS-COUNT)
                       MOVE CONN-USER-B TO
                           WS-CONN-USER-B(WS-CONNECTIONS-COUNT)
                   END-IF
           END-READ.

           IF WS-CONNECTIONS-EOF = "N"
               PERFORM 9260-READ-CONNECTIONS-LOOP
           END-IF.
           EXIT.

*>*****************************************************************
*> 9270-LOAD-NEXT-MSG-ID
*>   Scans MESSAGES.DAT to find the highest MSG-ID, then sets
*>   WS-MSG-NEXT-ID to max + 1 for the next message insert.
*>*****************************************************************
       9270-LOAD-NEXT-MSG-ID.
           MOVE 0 TO WS-MSG-NEXT-ID

           OPEN INPUT MESSAGES-FILE

           EVALUATE WS-MESSAGES-STATUS
               WHEN WS-CONST-FS-OK
                   PERFORM 9275-READ-MSG-ID-LOOP
                   CLOSE MESSAGES-FILE
               WHEN WS-CONST-FS-NOT-FOUND
                   MOVE 0 TO WS-MSG-NEXT-ID
               WHEN OTHER
                   MOVE 0 TO WS-MSG-NEXT-ID
           END-EVALUATE

           ADD 1 TO WS-MSG-NEXT-ID.
           EXIT.

*>*****************************************************************
*> 9275-READ-MSG-ID-LOOP
*>   Reads each message record and tracks the highest MSG-ID.
*>*****************************************************************
       9275-READ-MSG-ID-LOOP.
           READ MESSAGES-FILE
               AT END
                   EXIT PARAGRAPH
               NOT AT END
                   IF MSG-ID > WS-MSG-NEXT-ID
                       MOVE MSG-ID TO WS-MSG-NEXT-ID
                   END-IF
           END-READ

           PERFORM 9275-READ-MSG-ID-LOOP.
           EXIT.

*> *      *>*****************************************************************
*> *      *> 9210-READ-PENDING-LOOP: Read records into WS-PENDING-TABLE    *
*> *      *>*****************************************************************
       9210-READ-PENDING-LOOP.
           READ PENDING-FILE
               AT END
                   MOVE 1 TO WS-PENDING-EOF
               NOT AT END
                   IF WS-PENDING-COUNT < WS-MAX-PENDING
                       ADD 1 TO WS-PENDING-COUNT
                       MOVE PEND-SENDER-USERNAME TO
                           WS-PEND-SENDER-USERNAME(WS-PENDING-COUNT)
                       MOVE PEND-RECIPIENT-USERNAME TO
                           WS-PEND-RECIPIENT-USERNAME(WS-PENDING-COUNT)
                       MOVE PEND-STATUS TO
                           WS-PEND-STATUS(WS-PENDING-COUNT)
                   END-IF
           END-READ.

           IF WS-PENDING-EOF = 0
               PERFORM 9210-READ-PENDING-LOOP
           END-IF.
           EXIT.


*>*****************************************************************
*> *      *> 1160-READ-PROFILE-LOOP: Read profile records into memory      *
*> *      *>*****************************************************************
       1160-READ-PROFILE-LOOP.
           READ PROFILES-FILE
               AT END
                   MOVE 1 TO WS-EOF-FLAG
               NOT AT END
                   IF WS-PROFILE-COUNT < WS-MAX-ACCOUNTS
                       ADD 1 TO WS-PROFILE-COUNT
                       MOVE PROF-USERNAME TO
                           WS-PROF-USERNAME(WS-PROFILE-COUNT)
                       MOVE PROF-HAS-PROFILE TO
                           WS-HAS-PROFILE(WS-PROFILE-COUNT)
                       MOVE PROF-FIRST-NAME TO
                           WS-FIRST-NAME(WS-PROFILE-COUNT)
                       MOVE PROF-LAST-NAME TO
                           WS-LAST-NAME(WS-PROFILE-COUNT)
                       MOVE PROF-UNIVERSITY TO
                           WS-UNIVERSITY(WS-PROFILE-COUNT)
                       MOVE PROF-MAJOR TO
                           WS-MAJOR(WS-PROFILE-COUNT)
                       MOVE PROF-GRAD-YEAR TO
                           WS-GRAD-YEAR(WS-PROFILE-COUNT)
                       MOVE PROF-ABOUT-ME TO
                           WS-ABOUT-ME(WS-PROFILE-COUNT)
                       MOVE PROF-EXP-COUNT TO
                           WS-EXP-COUNT(WS-PROFILE-COUNT)
                       MOVE PROF-EDU-COUNT TO
                           WS-EDU-COUNT(WS-PROFILE-COUNT)

                       PERFORM 1161-COPY-EXPERIENCE-ENTRIES
                       PERFORM 1162-COPY-EDUCATION-ENTRIES
                   END-IF
           END-READ.

           IF WS-EOF-FLAG = 0
               PERFORM 1160-READ-PROFILE-LOOP
           ELSE
               MOVE 0 TO WS-EOF-FLAG
           END-IF.

*> *      *>*****************************************************************
*> *      *> 1161-COPY-EXPERIENCE-ENTRIES: Copy experience from file       *
*> *      *>*****************************************************************
       1161-COPY-EXPERIENCE-ENTRIES.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE PROF-EXP-TITLE(WS-DISPLAY-INDEX) TO
                   WS-EXP-TITLE(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EXP-COMPANY(WS-DISPLAY-INDEX) TO
                   WS-EXP-COMPANY(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EXP-DATES(WS-DISPLAY-INDEX) TO
                   WS-EXP-DATES(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EXP-DESC(WS-DISPLAY-INDEX) TO
                   WS-EXP-DESC(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 1162-COPY-EDUCATION-ENTRIES: Copy education from file         *
*> *      *>*****************************************************************
       1162-COPY-EDUCATION-ENTRIES.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE PROF-EDU-DEGREE(WS-DISPLAY-INDEX) TO
                   WS-EDU-DEGREE(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EDU-UNIVERSITY(WS-DISPLAY-INDEX) TO
                   WS-EDU-UNIVERSITY(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
               MOVE PROF-EDU-YEARS(WS-DISPLAY-INDEX) TO
                   WS-EDU-YEARS(WS-PROFILE-COUNT, WS-DISPLAY-INDEX)
           END-PERFORM.

