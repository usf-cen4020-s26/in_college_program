*>*****************************************************************
      *> FILE:    PROFILE.cpy
      *> PURPOSE: User profile creation, editing, viewing, and persistence.
      *>          Supports up to 3 work experience and 3 education entries.
      *>
      *> PARAGRAPHS:
      *>   4650-WRITE-PROFILES-FILE   - Rewrite entire PROFILES.DAT from table
      *>   4651-WRITE-ONE-PROFILE     - Write one profile entry to file
      *>   4652-SAVE-PROFILE-ENTRY    - Copy temp fields into profile table slot
      *>   7000-CREATE-EDIT-PROFILE   - Entry point; prompt all profile fields
      *>   7010-PROMPT-REQUIRED-FIELDS - Collect name, uni, major, grad year
      *>   7020-VALIDATE-GRAD-YEAR    - Ensure year is 4 digits, 1950-2050
      *>   7030-PROMPT-ABOUT-ME       - Optional bio (up to 200 chars)
      *>   7040-PROMPT-EXPERIENCE     - Loop for up to 3 experience blocks
      *>   7050-PROMPT-ONE-EXPERIENCE - Collect title, employer, dates, desc
      *>   7060-PROMPT-EDUCATION      - Loop for up to 3 education blocks
      *>   7070-PROMPT-ONE-EDUCATION  - Collect degree, school, years
      *>   7100-VIEW-PROFILE          - Display logged-in user's full profile
      *>   7110-DISPLAY-EXPERIENCE    - Print all experience entries
      *>   7120-DISPLAY-EDUCATION     - Print all education entries
      *>   7200-VIEW-OTHER-PROFILE    - Display another user's profile (from search)
      *>   7300-VIEW-OTHER-EXPERIENCE - Print experience for a found user
      *>   7400-VIEW-OTHER-EDUCATION  - Print education for a found user
      *>
      *> DEPENDENCIES:
      *>   WS-PROFILES.cpy   - WS-USER-PROFILES, WS-TEMP-* fields,
      *>                        WS-SEARCH-FOUND-INDEX, WS-PROFILE-COUNT
      *>   WS-ACCOUNTS.cpy   - WS-CURRENT-USER-INDEX, WS-ACCOUNT-INDEX
      *>   WS-CONSTANTS.cpy  - WS-CONST-MAX-EXPERIENCES, WS-CONST-MAX-EDUCATIONS
      *>   WS-IO-CONTROL.cpy - WS-EOF-FLAG, WS-PROGRAM-RUNNING, WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT, 8100-READ-INPUT, PROFILES-FILE
      *>*****************************************************************

*> *      *>*****************************************************************
*> *      *> 4650-WRITE-PROFILES-FILE: Persist all profiles to file        *
*> *      *>*****************************************************************
       4650-WRITE-PROFILES-FILE.
           OPEN OUTPUT PROFILES-FILE.

           PERFORM VARYING WS-ACCOUNT-INDEX FROM 1 BY 1
               UNTIL WS-ACCOUNT-INDEX > WS-PROFILE-COUNT

               MOVE WS-PROF-USERNAME(WS-ACCOUNT-INDEX) TO PROF-USERNAME
               MOVE WS-HAS-PROFILE(WS-ACCOUNT-INDEX) TO PROF-HAS-PROFILE
               MOVE WS-FIRST-NAME(WS-ACCOUNT-INDEX) TO PROF-FIRST-NAME
               MOVE WS-LAST-NAME(WS-ACCOUNT-INDEX) TO PROF-LAST-NAME
               MOVE WS-UNIVERSITY(WS-ACCOUNT-INDEX) TO PROF-UNIVERSITY
               MOVE WS-MAJOR(WS-ACCOUNT-INDEX) TO PROF-MAJOR
               MOVE WS-GRAD-YEAR(WS-ACCOUNT-INDEX) TO PROF-GRAD-YEAR
               MOVE WS-ABOUT-ME(WS-ACCOUNT-INDEX) TO PROF-ABOUT-ME
               MOVE WS-EXP-COUNT(WS-ACCOUNT-INDEX) TO PROF-EXP-COUNT
               MOVE WS-EDU-COUNT(WS-ACCOUNT-INDEX) TO PROF-EDU-COUNT

               PERFORM 4651-COPY-EXPERIENCE-TO-FILE
               PERFORM 4652-COPY-EDUCATION-TO-FILE

               WRITE PROFILE-RECORD
           END-PERFORM.

           CLOSE PROFILES-FILE.

*> *      *>*****************************************************************
*> *      *> 4651-COPY-EXPERIENCE-TO-FILE: Copy experience to file record  *
*> *      *>*****************************************************************
       4651-COPY-EXPERIENCE-TO-FILE.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE WS-EXP-TITLE(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-TITLE(WS-DISPLAY-INDEX)
               MOVE WS-EXP-COMPANY(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-COMPANY(WS-DISPLAY-INDEX)
               MOVE WS-EXP-DATES(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-DATES(WS-DISPLAY-INDEX)
               MOVE WS-EXP-DESC(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EXP-DESC(WS-DISPLAY-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 4652-COPY-EDUCATION-TO-FILE: Copy education to file record    *
*> *      *>*****************************************************************
       4652-COPY-EDUCATION-TO-FILE.
           PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
               UNTIL WS-DISPLAY-INDEX > 3

               MOVE WS-EDU-DEGREE(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EDU-DEGREE(WS-DISPLAY-INDEX)
               MOVE WS-EDU-UNIVERSITY(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EDU-UNIVERSITY(WS-DISPLAY-INDEX)
               MOVE WS-EDU-YEARS(WS-ACCOUNT-INDEX, WS-DISPLAY-INDEX) TO
                   PROF-EDU-YEARS(WS-DISPLAY-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7000-CREATE-EDIT-PROFILE: Create or edit user profile         *
*> *      *>*****************************************************************
       7000-CREATE-EDIT-PROFILE.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           IF WS-CURRENT-PROFILE-INDEX > 0 AND
               WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX) = 1
               MOVE "=== EDIT MY PROFILE ===" TO WS-OUTPUT-LINE
           ELSE
               MOVE "=== CREATE MY PROFILE ===" TO WS-OUTPUT-LINE
           END-IF.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7200-GET-REQUIRED-FIELDS.

           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM 7300-GET-OPTIONAL-FIELDS.

           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM 7400-SAVE-PROFILE-DATA.
           PERFORM 4650-WRITE-PROFILES-FILE.

           MOVE "Profile saved successfully!" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7100-VIEW-PROFILE: Display user's profile                     *
*> *      *>*****************************************************************
       7100-VIEW-PROFILE.
           MOVE " " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           IF WS-CURRENT-PROFILE-INDEX = 0 OR
               WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX) = 0
               MOVE "You have not created a profile yet."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Please use 'Create/Edit My Profile' option first."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF.

           MOVE "=== YOUR PROFILE ===" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE WS-CURRENT-PROFILE-INDEX TO WS-DISPLAY-PROFILE-INDEX.

           PERFORM 7110-DISPLAY-BASIC-INFO.

           PERFORM 7120-DISPLAY-ABOUT-ME.

           PERFORM 7130-DISPLAY-EXPERIENCE.

           PERFORM 7140-DISPLAY-EDUCATION.

           MOVE "--------------------" TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7110-DISPLAY-BASIC-INFO: Display required profile fields      *
*> *      *>*****************************************************************
       7110-DISPLAY-BASIC-INFO.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "Name: "
               FUNCTION TRIM(WS-FIRST-NAME(WS-DISPLAY-PROFILE-INDEX))
               " "
               FUNCTION TRIM(WS-LAST-NAME(WS-DISPLAY-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "University: "
               FUNCTION TRIM(WS-UNIVERSITY(WS-DISPLAY-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "Major: "
               FUNCTION TRIM(WS-MAJOR(WS-DISPLAY-PROFILE-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "Graduation Year: "
               WS-GRAD-YEAR(WS-DISPLAY-PROFILE-INDEX)
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7120-DISPLAY-ABOUT-ME: Display About Me section               *
*> *      *>*****************************************************************
       7120-DISPLAY-ABOUT-ME.
           IF WS-ABOUT-ME(WS-DISPLAY-PROFILE-INDEX) NOT = SPACES
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "About Me: "
                   FUNCTION TRIM(WS-ABOUT-ME(WS-DISPLAY-PROFILE-INDEX))
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "About Me: None" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7130-DISPLAY-EXPERIENCE: Display all experience entries       *
*> *      *>*****************************************************************
       7130-DISPLAY-EXPERIENCE.
           IF WS-EXP-COUNT(WS-DISPLAY-PROFILE-INDEX) > 0
               MOVE "Experience:" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
                   UNTIL WS-DISPLAY-INDEX >
                       WS-EXP-COUNT(WS-DISPLAY-PROFILE-INDEX)

                   PERFORM 7131-DISPLAY-SINGLE-EXPERIENCE
               END-PERFORM
           ELSE
               MOVE "Experience: None" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7131-DISPLAY-SINGLE-EXPERIENCE: Display one experience entry  *
*> *      *>*****************************************************************
       7131-DISPLAY-SINGLE-EXPERIENCE.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Title: "
               FUNCTION TRIM(WS-EXP-TITLE(WS-DISPLAY-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Company/Organization: "
               FUNCTION TRIM(WS-EXP-COMPANY(WS-DISPLAY-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Dates: "
               FUNCTION TRIM(WS-EXP-DATES(WS-DISPLAY-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           IF WS-EXP-DESC(WS-DISPLAY-PROFILE-INDEX, WS-DISPLAY-INDEX)
               NOT = SPACES
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "  Description: "
                   FUNCTION TRIM(WS-EXP-DESC(WS-DISPLAY-PROFILE-INDEX,
                       WS-DISPLAY-INDEX))
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7140-DISPLAY-EDUCATION: Display all education entries         *
*> *      *>*****************************************************************
       7140-DISPLAY-EDUCATION.
           IF WS-EDU-COUNT(WS-DISPLAY-PROFILE-INDEX) > 0
               MOVE "Education:" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
                   UNTIL WS-DISPLAY-INDEX >
                       WS-EDU-COUNT(WS-DISPLAY-PROFILE-INDEX)

                   PERFORM 7141-DISPLAY-SINGLE-EDUCATION
               END-PERFORM
           ELSE
               MOVE "Education: None" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7141-DISPLAY-SINGLE-EDUCATION: Display one education entry    *
*> *      *>*****************************************************************
       7141-DISPLAY-SINGLE-EDUCATION.
           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Degree: "
               FUNCTION TRIM(WS-EDU-DEGREE(WS-DISPLAY-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  University: "
               FUNCTION TRIM(WS-EDU-UNIVERSITY(WS-DISPLAY-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE SPACES TO WS-OUTPUT-LINE.
           STRING "  Years: "
               FUNCTION TRIM(WS-EDU-YEARS(WS-DISPLAY-PROFILE-INDEX,
                   WS-DISPLAY-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7200-GET-REQUIRED-FIELDS: Collect required profile fields     *
*> *      *>*****************************************************************
       7200-GET-REQUIRED-FIELDS.
           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter First Name: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-FIRST-NAME
                   IF WS-TEMP-FIRST-NAME = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-FIRST-NAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Last Name: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-LAST-NAME
                   IF WS-TEMP-LAST-NAME = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-LAST-NAME TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter University/College Attended: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-UNIVERSITY
                   IF WS-TEMP-UNIVERSITY = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-UNIVERSITY TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           MOVE 0 TO WS-INPUT-VALID.
           MOVE 0 TO WS-EOF-REACHED.
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Major: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 1 TO WS-EOF-REACHED
                   MOVE 1 TO WS-INPUT-VALID
               ELSE
                   MOVE INPUT-RECORD TO WS-TEMP-MAJOR
                   IF WS-TEMP-MAJOR = SPACES
                       MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   ELSE
                       MOVE 1 TO WS-INPUT-VALID
                   END-IF
               END-IF
           END-PERFORM
           IF WS-EOF-REACHED = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE WS-TEMP-MAJOR TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 7210-GET-VALID-GRAD-YEAR.

*> *      *>*****************************************************************
*> *      *> 7210-GET-VALID-GRAD-YEAR: Get and validate graduation year    *
*> *      *>*****************************************************************
       7210-GET-VALID-GRAD-YEAR.
           MOVE 0 TO WS-YEAR-VALID.

           PERFORM UNTIL WS-YEAR-VALID = 1
               MOVE "Enter Graduation Year (YYYY): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-TEMP-GRAD-YEAR
               MOVE WS-TEMP-GRAD-YEAR TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 7220-VALIDATE-YEAR

               IF WS-YEAR-VALID = 0
                   MOVE "Invalid year. Must be 4-digit year (1950-2050)."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               END-IF
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7220-VALIDATE-YEAR: Validate graduation year format           *
*> *      *>*****************************************************************
       7220-VALIDATE-YEAR.
           MOVE 0 TO WS-YEAR-VALID.
           MOVE 1 TO WS-YEAR-NUMERIC.

           IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-GRAD-YEAR)) NOT = 4
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-YEAR-INDEX FROM 1 BY 1
               UNTIL WS-YEAR-INDEX > 4
               MOVE WS-TEMP-GRAD-YEAR(WS-YEAR-INDEX:1) TO WS-TEMP-CHAR
               IF WS-TEMP-CHAR < "0" OR WS-TEMP-CHAR > "9"
                   MOVE 0 TO WS-YEAR-NUMERIC
               END-IF
           END-PERFORM.

           IF WS-YEAR-NUMERIC = 0
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-TEMP-GRAD-YEAR TO WS-TEMP-YEAR.
           IF WS-TEMP-YEAR >= 1950 AND WS-TEMP-YEAR <= 2050
               MOVE 1 TO WS-YEAR-VALID
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7300-GET-OPTIONAL-FIELDS: Collect optional profile fields     *
*> *      *>*****************************************************************
       7300-GET-OPTIONAL-FIELDS.
           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
               TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           MOVE INPUT-RECORD TO WS-TEMP-ABOUT-ME.
           IF WS-TEMP-ABOUT-ME NOT = SPACES
               MOVE WS-TEMP-ABOUT-ME TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "(skipped)" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

           PERFORM 7310-GET-EXPERIENCE-ENTRIES.

           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM 7320-GET-EDUCATION-ENTRIES.

*> *      *>*****************************************************************
*> *      *> 7310-GET-EXPERIENCE-ENTRIES: Collect experience entries       *
*> *      *>*****************************************************************
       7310-GET-EXPERIENCE-ENTRIES.
           MOVE 0 TO WS-EXP-LOOP-INDEX.
           MOVE "ADD" TO WS-CONTINUE-ADDING.

           PERFORM UNTIL WS-EXP-LOOP-INDEX >= 3
               OR WS-CONTINUE-ADDING = "DONE"
               OR WS-EOF-FLAG = 1

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Add Experience (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-CONTINUE-ADDING
               MOVE WS-CONTINUE-ADDING TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-CONTINUE-ADDING = "DONE"
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-EXP-LOOP-INDEX
               PERFORM 7311-GET-SINGLE-EXPERIENCE
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7311-GET-SINGLE-EXPERIENCE: Collect one experience entry      *
*> *      *>*****************************************************************
       7311-GET-SINGLE-EXPERIENCE.
           STRING "Experience #" WS-EXP-LOOP-INDEX " - Title: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-TITLE(WS-EXP-LOOP-INDEX).
           MOVE WS-TEMP-EXP-TITLE(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Experience #" WS-EXP-LOOP-INDEX
               " - Company/Organization: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-COMPANY(WS-EXP-LOOP-INDEX).
           MOVE WS-TEMP-EXP-COMPANY(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Experience #" WS-EXP-LOOP-INDEX
               " - Dates (e.g., Summer 2024): "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-DATES(WS-EXP-LOOP-INDEX).
           MOVE WS-TEMP-EXP-DATES(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Experience #" WS-EXP-LOOP-INDEX
               " - Description (optional, max 100 chars,"
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.
           MOVE "blank to skip): " TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX).
           IF WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX) NOT = SPACES
               MOVE WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "(skipped)" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF.

*> *      *>*****************************************************************
*> *      *> 7320-GET-EDUCATION-ENTRIES: Collect education entries         *
*> *      *>*****************************************************************
       7320-GET-EDUCATION-ENTRIES.
           MOVE 0 TO WS-EDU-LOOP-INDEX.
           MOVE "ADD" TO WS-CONTINUE-ADDING.

           PERFORM UNTIL WS-EDU-LOOP-INDEX >= 3
               OR WS-CONTINUE-ADDING = "DONE"
               OR WS-EOF-FLAG = 1

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Add Education (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-CONTINUE-ADDING
               MOVE WS-CONTINUE-ADDING TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-CONTINUE-ADDING = "DONE"
                   EXIT PERFORM
               END-IF

      *>       If the user did not enter "DONE", proceed to get education entry -> this encompasses blank lines and other inputs.

               ADD 1 TO WS-EDU-LOOP-INDEX
               PERFORM 7321-GET-SINGLE-EDUCATION
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7321-GET-SINGLE-EDUCATION: Collect one education entry        *
*> *      *>*****************************************************************
       7321-GET-SINGLE-EDUCATION.
           STRING "Education #" WS-EDU-LOOP-INDEX " - Degree: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EDU-DEGREE(WS-EDU-LOOP-INDEX).
           MOVE WS-TEMP-EDU-DEGREE(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Education #" WS-EDU-LOOP-INDEX
               " - University/College: "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EDU-UNIVERSITY(WS-EDU-LOOP-INDEX).
           MOVE WS-TEMP-EDU-UNIVERSITY(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

           STRING "Education #" WS-EDU-LOOP-INDEX
               " - Years Attended (e.g., 2023-2025): "
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING.
           PERFORM 8000-WRITE-OUTPUT.

           PERFORM 8100-READ-INPUT.
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF.
           MOVE INPUT-RECORD TO WS-TEMP-EDU-YEARS(WS-EDU-LOOP-INDEX).
           MOVE WS-TEMP-EDU-YEARS(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
           PERFORM 8000-WRITE-OUTPUT.

*> *      *>*****************************************************************
*> *      *> 7400-SAVE-PROFILE-DATA: Save profile data to memory           *
*> *      *>*****************************************************************
       7400-SAVE-PROFILE-DATA.
           IF WS-CURRENT-PROFILE-INDEX = 0
               ADD 1 TO WS-PROFILE-COUNT
               MOVE WS-PROFILE-COUNT TO WS-CURRENT-PROFILE-INDEX
               MOVE WS-USERNAME(WS-CURRENT-USER-INDEX) TO
                   WS-PROF-USERNAME(WS-CURRENT-PROFILE-INDEX)
           END-IF.

           MOVE 1 TO WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX).

           MOVE WS-TEMP-FIRST-NAME TO
               WS-FIRST-NAME(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-LAST-NAME TO
               WS-LAST-NAME(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-UNIVERSITY TO
               WS-UNIVERSITY(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-MAJOR TO
               WS-MAJOR(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-GRAD-YEAR TO
               WS-GRAD-YEAR(WS-CURRENT-PROFILE-INDEX).
           MOVE WS-TEMP-ABOUT-ME TO
               WS-ABOUT-ME(WS-CURRENT-PROFILE-INDEX).

           MOVE WS-EXP-LOOP-INDEX TO
               WS-EXP-COUNT(WS-CURRENT-PROFILE-INDEX).
           PERFORM 7410-SAVE-EXPERIENCE-ENTRIES.

           MOVE WS-EDU-LOOP-INDEX TO
               WS-EDU-COUNT(WS-CURRENT-PROFILE-INDEX).
           PERFORM 7420-SAVE-EDUCATION-ENTRIES.

*> *      *>*****************************************************************
*> *      *> 7410-SAVE-EXPERIENCE-ENTRIES: Copy experience to profile      *
*> *      *>*****************************************************************
       7410-SAVE-EXPERIENCE-ENTRIES.
           PERFORM VARYING WS-SAVE-INDEX FROM 1 BY 1
               UNTIL WS-SAVE-INDEX > WS-EXP-LOOP-INDEX
               MOVE WS-TEMP-EXP-TITLE(WS-SAVE-INDEX) TO
                   WS-EXP-TITLE(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EXP-COMPANY(WS-SAVE-INDEX) TO
                   WS-EXP-COMPANY(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EXP-DATES(WS-SAVE-INDEX) TO
                   WS-EXP-DATES(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EXP-DESC(WS-SAVE-INDEX) TO
                   WS-EXP-DESC(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
           END-PERFORM.

*> *      *>*****************************************************************
*> *      *> 7420-SAVE-EDUCATION-ENTRIES: Copy education to profile        *
*> *      *>*****************************************************************
       7420-SAVE-EDUCATION-ENTRIES.
           PERFORM VARYING WS-SAVE-INDEX FROM 1 BY 1
               UNTIL WS-SAVE-INDEX > WS-EDU-LOOP-INDEX
               MOVE WS-TEMP-EDU-DEGREE(WS-SAVE-INDEX) TO
                   WS-EDU-DEGREE(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EDU-UNIVERSITY(WS-SAVE-INDEX) TO
                   WS-EDU-UNIVERSITY(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
               MOVE WS-TEMP-EDU-YEARS(WS-SAVE-INDEX) TO
                   WS-EDU-YEARS(WS-CURRENT-PROFILE-INDEX, WS-SAVE-INDEX)
           END-PERFORM.
