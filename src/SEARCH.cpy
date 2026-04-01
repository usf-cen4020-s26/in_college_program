      *> ============================================================
      *> SEARCH_SRC.cpy - Find someone you know by name
      *> Paragraphs: 7500-FIND-SOMEONE-YOU-KNOW and helpers
      *> ============================================================
       7500-FIND-SOMEONE-YOU-KNOW.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "Enter the full name of the person you are looking for: "
               TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.

           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF.

           MOVE INPUT-RECORD TO WS-SEARCH-NAME.
           MOVE FUNCTION TRIM(WS-SEARCH-NAME) TO WS-SEARCH-NAME.
           MOVE WS-SEARCH-NAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7510-SEARCH-FOR-USER.

           IF WS-USER-FOUND = 1
               PERFORM 7520-DISPLAY-FOUND-PROFILE
           ELSE
               MOVE "No one by that name could be found." TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7510-SEARCH-FOR-USER: Search profiles for matching name      *
*> *      *> USER STORY (Epic 3): Exact match search by full name         *
*> *      *>*****************************************************************
       7510-SEARCH-FOR-USER.
           MOVE 0 TO WS-USER-FOUND.
           MOVE 0 TO WS-SEARCH-FOUND-INDEX.
           MOVE SPACES TO WS-SEARCH-FIRST-NAME.
           MOVE SPACES TO WS-SEARCH-LAST-NAME.

           UNSTRING WS-SEARCH-NAME
               DELIMITED BY " "
               INTO WS-SEARCH-FIRST-NAME
                    WS-SEARCH-LAST-NAME
           END-UNSTRING.

           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT
                   OR WS-USER-FOUND = 1

               IF WS-HAS-PROFILE(WS-ACCOUNT-INDEX) = 1
                   IF FUNCTION TRIM(WS-FIRST-NAME(WS-ACCOUNT-INDEX)) =
                       FUNCTION TRIM(WS-SEARCH-FIRST-NAME)
                       AND FUNCTION TRIM(WS-LAST-NAME(WS-ACCOUNT-INDEX)) =
                       FUNCTION TRIM(WS-SEARCH-LAST-NAME)
                       MOVE 1 TO WS-USER-FOUND
                       MOVE WS-ACCOUNT-INDEX TO WS-SEARCH-FOUND-INDEX
                   END-IF
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7520-DISPLAY-FOUND-PROFILE: Display profile of found user    *
*> *      *> USER STORY (Epic 3): Display full profile when user found    *
*> *      *>*****************************************************************
       7520-DISPLAY-FOUND-PROFILE.
           MOVE "--- Found User Profile ---" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE WS-SEARCH-FOUND-INDEX TO WS-DISPLAY-PROFILE-INDEX.
           MOVE WS-SEARCH-FOUND-INDEX TO WS-SENDREQ-TARGET-INDEX

           PERFORM 7110-DISPLAY-BASIC-INFO.

           PERFORM 7120-DISPLAY-ABOUT-ME.

           PERFORM 7130-DISPLAY-EXPERIENCE.

           PERFORM 7140-DISPLAY-EDUCATION.

           MOVE "-------------------------" TO WS-OUTPUT-LINE.

           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7600-SEND-REQUEST-MENU.
           EXIT.
