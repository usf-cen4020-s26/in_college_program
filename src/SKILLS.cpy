*>*****************************************************************
      *> FILE:    SKILLS.cpy
      *> PURPOSE: "Learn a New Skill" submenu — placeholder framework.
      *>          Displays a 5-item skill menu; each option currently
      *>          prints a placeholder message. Reserved for a future epic.
      *>
      *> PARAGRAPHS:
      *>   6000-SKILLS-MENU - Entry point; loop menu until option 6 (Back);
      *>                      options 1-5 each print a placeholder message
      *>
      *> DEPENDENCIES:
      *>   WS-IO-CONTROL.cpy - WS-SKILL-CHOICE, WS-EOF-FLAG,
      *>                        WS-PROGRAM-RUNNING, WS-OUTPUT-LINE
      *>   main.cob          - 8000-WRITE-OUTPUT, 8100-READ-INPUT
      *>*****************************************************************
       6000-SKILLS-MENU.
           MOVE "1" TO WS-SKILL-CHOICE.

           PERFORM UNTIL WS-SKILL-CHOICE = "6"
               OR WS-PROGRAM-RUNNING = 0
               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "=== LEARN A NEW SKILL ===" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "1. Skill 1" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "2. Skill 2" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "3. Skill 3" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "4. Skill 4" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "5. Skill 5" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "6. Go Back" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter choice (1-6): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT

               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE INPUT-RECORD TO WS-SKILL-CHOICE
               MOVE WS-SKILL-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-SKILL-CHOICE = "1" OR WS-SKILL-CHOICE = "2" OR
                   WS-SKILL-CHOICE = "3" OR WS-SKILL-CHOICE = "4" OR
                   WS-SKILL-CHOICE = "5"
                   MOVE "This skill is under construction." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   IF WS-SKILL-CHOICE NOT = "6"
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   END-IF
               END-IF
           END-PERFORM.
