IDENTIFICATION DIVISION.
PROGRAM-ID. PROFILE-MODULE.

*> *      *>*****************************************************************
*> *      *> PROFILE-MODULE: Create/edit and view profile flows           *
*> *      *> USER STORY (Epic 2): Profile creation and viewing            *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    PERFORM 7000-CREATE-EDIT-PROFILE.
    GOBACK.

ENTRY "PROFILE-VIEW" USING LS-CONTEXT.
    PERFORM 7100-VIEW-PROFILE.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 7000-CREATE-EDIT-PROFILE: Create or edit user profile         *
*> *      *> USER STORY (Epic 2): Profile creation and editing             *
*> *      *>*****************************************************************
    7000-CREATE-EDIT-PROFILE.
        MOVE " " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        IF WS-CURRENT-PROFILE-INDEX > 0 AND
            WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX) = 1
            MOVE "=== EDIT MY PROFILE ===" TO WS-OUTPUT-LINE
        ELSE
            MOVE "=== CREATE MY PROFILE ===" TO WS-OUTPUT-LINE
        END-IF.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        PERFORM 7200-GET-REQUIRED-FIELDS.

        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.

        PERFORM 7300-GET-OPTIONAL-FIELDS.

        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.

        PERFORM 7400-SAVE-PROFILE-DATA.
        MOVE "SAVE-PROF" TO WS-DATA-ACTION
        CALL "DATA-STORE" USING WS-DATA-ACTION LS-CONTEXT.

        MOVE "Profile saved successfully!" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

*> *      *>*****************************************************************
*> *      *> 7100-VIEW-PROFILE: Display user's profile                     *
*> *      *> USER STORY (Epic 2): View profile information                 *
*> *      *>*****************************************************************
    7100-VIEW-PROFILE.
        MOVE " " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        IF WS-CURRENT-PROFILE-INDEX = 0 OR
            WS-HAS-PROFILE(WS-CURRENT-PROFILE-INDEX) = 0
            MOVE "You have not created a profile yet."
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "Please use 'Create/Edit My Profile' option first."
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            EXIT PARAGRAPH
        END-IF.

        MOVE "=== YOUR PROFILE ===" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE WS-CURRENT-PROFILE-INDEX TO WS-DISPLAY-PROFILE-INDEX.

        CALL "PROFILE-DISPLAY" USING LS-CONTEXT.

        MOVE "--------------------" TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

*> *      *>*****************************************************************
*> *      *> 7200-GET-REQUIRED-FIELDS: Collect required profile fields     *
*> *      *>*****************************************************************
    7200-GET-REQUIRED-FIELDS.
        MOVE 0 TO WS-INPUT-VALID.
        MOVE 0 TO WS-EOF-REACHED.
        PERFORM UNTIL WS-INPUT-VALID = 1
            MOVE "Enter First Name: " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                MOVE 1 TO WS-EOF-REACHED
                MOVE 1 TO WS-INPUT-VALID
            ELSE
                MOVE WS-INPUT-LINE TO WS-TEMP-FIRST-NAME
                IF WS-TEMP-FIRST-NAME = SPACES
                    MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                ELSE
                    MOVE 1 TO WS-INPUT-VALID
                END-IF
            END-IF
        END-PERFORM
        IF WS-EOF-REACHED = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-TEMP-FIRST-NAME TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE 0 TO WS-INPUT-VALID.
        MOVE 0 TO WS-EOF-REACHED.
        PERFORM UNTIL WS-INPUT-VALID = 1
            MOVE "Enter Last Name: " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                MOVE 1 TO WS-EOF-REACHED
                MOVE 1 TO WS-INPUT-VALID
            ELSE
                MOVE WS-INPUT-LINE TO WS-TEMP-LAST-NAME
                IF WS-TEMP-LAST-NAME = SPACES
                    MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                ELSE
                    MOVE 1 TO WS-INPUT-VALID
                END-IF
            END-IF
        END-PERFORM
        IF WS-EOF-REACHED = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-TEMP-LAST-NAME TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE 0 TO WS-INPUT-VALID.
        MOVE 0 TO WS-EOF-REACHED.
        PERFORM UNTIL WS-INPUT-VALID = 1
            MOVE "Enter University/College Attended: " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                MOVE 1 TO WS-EOF-REACHED
                MOVE 1 TO WS-INPUT-VALID
            ELSE
                MOVE WS-INPUT-LINE TO WS-TEMP-UNIVERSITY
                IF WS-TEMP-UNIVERSITY = SPACES
                    MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                ELSE
                    MOVE 1 TO WS-INPUT-VALID
                END-IF
            END-IF
        END-PERFORM
        IF WS-EOF-REACHED = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-TEMP-UNIVERSITY TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE 0 TO WS-INPUT-VALID.
        MOVE 0 TO WS-EOF-REACHED.
        PERFORM UNTIL WS-INPUT-VALID = 1
            MOVE "Enter Major: " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                MOVE 1 TO WS-EOF-REACHED
                MOVE 1 TO WS-INPUT-VALID
            ELSE
                MOVE WS-INPUT-LINE TO WS-TEMP-MAJOR
                IF WS-TEMP-MAJOR = SPACES
                    MOVE "Invalid input. Please try again." TO WS-OUTPUT-LINE
                    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                ELSE
                    MOVE 1 TO WS-INPUT-VALID
                END-IF
            END-IF
        END-PERFORM
        IF WS-EOF-REACHED = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-TEMP-MAJOR TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        PERFORM 7210-GET-VALID-GRAD-YEAR.

*> *      *>*****************************************************************
*> *      *> 7210-GET-VALID-GRAD-YEAR: Get and validate graduation year    *
*> *      *>*****************************************************************
    7210-GET-VALID-GRAD-YEAR.
        MOVE 0 TO WS-YEAR-VALID.

        PERFORM UNTIL WS-YEAR-VALID = 1
            MOVE "Enter Graduation Year (YYYY): " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                EXIT PERFORM
            END-IF

            MOVE WS-INPUT-LINE TO WS-TEMP-GRAD-YEAR
            MOVE WS-TEMP-GRAD-YEAR TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            PERFORM 7220-VALIDATE-YEAR

            IF WS-YEAR-VALID = 0
                MOVE "Invalid year. Must be 4-digit year (1950-2050)."
                    TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
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
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.

        MOVE WS-INPUT-LINE TO WS-TEMP-ABOUT-ME.
        IF WS-TEMP-ABOUT-ME NOT = SPACES
            MOVE WS-TEMP-ABOUT-ME TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        ELSE
            MOVE "(skipped)" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
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
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "Add Experience (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):"
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                EXIT PERFORM
            END-IF

            MOVE WS-INPUT-LINE TO WS-CONTINUE-ADDING
            MOVE WS-CONTINUE-ADDING TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

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
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EXP-TITLE(WS-EXP-LOOP-INDEX).
        MOVE WS-TEMP-EXP-TITLE(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        STRING "Experience #" WS-EXP-LOOP-INDEX
            " - Company/Organization: "
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EXP-COMPANY(WS-EXP-LOOP-INDEX).
        MOVE WS-TEMP-EXP-COMPANY(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        STRING "Experience #" WS-EXP-LOOP-INDEX
            " - Dates (e.g., Summer 2024): "
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EXP-DATES(WS-EXP-LOOP-INDEX).
        MOVE WS-TEMP-EXP-DATES(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        STRING "Experience #" WS-EXP-LOOP-INDEX
            " - Description (optional, max 100 chars,"
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
        MOVE "blank to skip): " TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX).
        IF WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX) NOT = SPACES
            MOVE WS-TEMP-EXP-DESC(WS-EXP-LOOP-INDEX) TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        ELSE
            MOVE "(skipped)" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
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
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "Add Education (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):"
                TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG
            IF WS-EOF-FLAG = 1
                EXIT PERFORM
            END-IF

            MOVE WS-INPUT-LINE TO WS-CONTINUE-ADDING
            MOVE WS-CONTINUE-ADDING TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

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
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EDU-DEGREE(WS-EDU-LOOP-INDEX).
        MOVE WS-TEMP-EDU-DEGREE(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        STRING "Education #" WS-EDU-LOOP-INDEX
            " - University/College: "
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EDU-UNIVERSITY(WS-EDU-LOOP-INDEX).
        MOVE WS-TEMP-EDU-UNIVERSITY(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        STRING "Education #" WS-EDU-LOOP-INDEX
            " - Years Attended (e.g., 2023-2025): "
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG.
        IF WS-EOF-FLAG = 1
            EXIT PARAGRAPH
        END-IF.
        MOVE WS-INPUT-LINE TO WS-TEMP-EDU-YEARS(WS-EDU-LOOP-INDEX).
        MOVE WS-TEMP-EDU-YEARS(WS-EDU-LOOP-INDEX) TO WS-OUTPUT-LINE.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

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
