      *> ============================================================
      *> SKILLS_SRC.cpy - Skills menu (under construction)
      *> Paragraphs: 6000-SKILLS-MENU
      *> ============================================================
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
