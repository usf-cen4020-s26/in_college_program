IDENTIFICATION DIVISION.
PROGRAM-ID. SKILLS-MENU.

*> *      *>*****************************************************************
*> *      *> SKILLS-MENU: Learn a new skill submenu                       *
*> *      *> USER STORY (Epic 1): Skills menu scaffolding                  *
*> *      *>*****************************************************************

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-CONTEXT.
    COPY "context.cpy".

PROCEDURE DIVISION USING LS-CONTEXT.
    PERFORM 6000-SKILLS-MENU.
    GOBACK.

*> *      *>*****************************************************************
*> *      *> 6000-SKILLS-MENU: Learn a new skill submenu                   *
*> *      *>*****************************************************************
    6000-SKILLS-MENU.
        MOVE "1" TO WS-SKILL-CHOICE.

        PERFORM UNTIL WS-SKILL-CHOICE = "6"
            OR WS-PROGRAM-RUNNING = 0
            MOVE " " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "=== LEARN A NEW SKILL ===" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "1. Skill 1" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "2. Skill 2" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "3. Skill 3" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "4. Skill 4" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "5. Skill 5" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "6. Go Back" TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            MOVE "Enter choice (1-6): " TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            CALL "IO-SERVICE" USING "READ " WS-INPUT-LINE WS-EOF-FLAG

            IF WS-EOF-FLAG = 1
                MOVE 0 TO WS-PROGRAM-RUNNING
                EXIT PERFORM
            END-IF

            MOVE WS-INPUT-LINE TO WS-SKILL-CHOICE
            MOVE WS-SKILL-CHOICE TO WS-OUTPUT-LINE
            CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG

            IF WS-SKILL-CHOICE = "1" OR WS-SKILL-CHOICE = "2" OR
                WS-SKILL-CHOICE = "3" OR WS-SKILL-CHOICE = "4" OR
                WS-SKILL-CHOICE = "5"
                MOVE "This skill is under construction." TO WS-OUTPUT-LINE
                CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
            ELSE
                IF WS-SKILL-CHOICE NOT = "6"
                    MOVE "Invalid choice. Please try again."
                        TO WS-OUTPUT-LINE
                    CALL "IO-SERVICE" USING "WRITE" WS-OUTPUT-LINE WS-EOF-FLAG
                END-IF
            END-IF
        END-PERFORM.
