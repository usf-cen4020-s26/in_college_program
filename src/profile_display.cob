IDENTIFICATION DIVISION.
PROGRAM-ID. PROFILE-DISPLAY.

*> *      *>*****************************************************************
*> *      *> PROFILE-DISPLAY: Render profile sections                     *
*> *      *> USER STORY (Epic 2): View profile information                 *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    PERFORM 7110-DISPLAY-BASIC-INFO.
    PERFORM 7120-DISPLAY-ABOUT-ME.
    PERFORM 7130-DISPLAY-EXPERIENCE.
    PERFORM 7140-DISPLAY-EDUCATION.
    GOBACK.

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
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "University: "
            FUNCTION TRIM(WS-UNIVERSITY(WS-DISPLAY-PROFILE-INDEX))
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "Major: "
            FUNCTION TRIM(WS-MAJOR(WS-DISPLAY-PROFILE-INDEX))
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "Graduation Year: "
            WS-GRAD-YEAR(WS-DISPLAY-PROFILE-INDEX)
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

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
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        ELSE
            MOVE "About Me: None" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        END-IF.

*> *      *>*****************************************************************
*> *      *> 7130-DISPLAY-EXPERIENCE: Display all experience entries       *
*> *      *>*****************************************************************
    7130-DISPLAY-EXPERIENCE.
        IF WS-EXP-COUNT(WS-DISPLAY-PROFILE-INDEX) > 0
            MOVE "Experience:" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
                UNTIL WS-DISPLAY-INDEX >
                    WS-EXP-COUNT(WS-DISPLAY-PROFILE-INDEX)

                PERFORM 7131-DISPLAY-SINGLE-EXPERIENCE
            END-PERFORM
        ELSE
            MOVE "Experience: None" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
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
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "  Company/Organization: "
            FUNCTION TRIM(WS-EXP-COMPANY(WS-DISPLAY-PROFILE-INDEX,
                WS-DISPLAY-INDEX))
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "  Dates: "
            FUNCTION TRIM(WS-EXP-DATES(WS-DISPLAY-PROFILE-INDEX,
                WS-DISPLAY-INDEX))
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        IF WS-EXP-DESC(WS-DISPLAY-PROFILE-INDEX, WS-DISPLAY-INDEX)
            NOT = SPACES
            MOVE SPACES TO WS-OUTPUT-LINE
            STRING "  Description: "
                FUNCTION TRIM(WS-EXP-DESC(WS-DISPLAY-PROFILE-INDEX,
                    WS-DISPLAY-INDEX))
                DELIMITED BY SIZE INTO WS-OUTPUT-LINE
            END-STRING
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
        END-IF.

*> *      *>*****************************************************************
*> *      *> 7140-DISPLAY-EDUCATION: Display all education entries         *
*> *      *>*****************************************************************
    7140-DISPLAY-EDUCATION.
        IF WS-EDU-COUNT(WS-DISPLAY-PROFILE-INDEX) > 0
            MOVE "Education:" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            PERFORM VARYING WS-DISPLAY-INDEX FROM 1 BY 1
                UNTIL WS-DISPLAY-INDEX >
                    WS-EDU-COUNT(WS-DISPLAY-PROFILE-INDEX)

                PERFORM 7141-DISPLAY-SINGLE-EDUCATION
            END-PERFORM
        ELSE
            MOVE "Education: None" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
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
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "  University: "
            FUNCTION TRIM(WS-EDU-UNIVERSITY(WS-DISPLAY-PROFILE-INDEX,
                WS-DISPLAY-INDEX))
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.

        MOVE SPACES TO WS-OUTPUT-LINE.
        STRING "  Years: "
            FUNCTION TRIM(WS-EDU-YEARS(WS-DISPLAY-PROFILE-INDEX,
                WS-DISPLAY-INDEX))
            DELIMITED BY SIZE INTO WS-OUTPUT-LINE
        END-STRING.
        CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG.
