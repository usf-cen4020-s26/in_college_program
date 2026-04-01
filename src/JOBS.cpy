>>SOURCE FORMAT FREE
*> *      *>*****************************************************************
*> *      *> 5300-JOB-SEARCH-MENU: Job Search/Internship submenu loop
*> *      *> Displays submenu and handles user input for job search/internship options
*> *      *>*****************************************************************
       5300-JOB-SEARCH-MENU.
           MOVE "1" TO WS-JOB-MENU-CHOICE

           PERFORM UNTIL WS-JOB-MENU-CHOICE = "4"
               OR WS-PROGRAM-RUNNING = 0

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "--- Job Search/Internship Menu ---"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "1. Post a Job/Internship" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "2. Browse Jobs/Internships" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "3. View My Applications" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "4. Back to Main Menu" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter your choice (1-4): " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-MENU-CHOICE
               MOVE WS-JOB-MENU-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               EVALUATE WS-JOB-MENU-CHOICE
                   WHEN "1"
                       PERFORM 5310-POST-JOB
                   WHEN "2"
                       PERFORM 5320-BROWSE-JOBS
                   WHEN "3"
                       PERFORM 5340-VIEW-MY-APPLICATIONS
                   WHEN "4"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
               END-EVALUATE

           END-PERFORM.
           EXIT.

      *>*****************************************************************
      *> 5310-POST-JOB: Full job posting input and save flow
      *> 1) Prompts for Title, Description, Employer, Location
      *> 2) Validates required fields, re-prompts on blank input
      *> 3) Optional salary via NONE convention
      *> 4) Prints confirmation and separator after save
      *>*****************************************************************
       5310-POST-JOB.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "--- Post a New Job/Internship ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

      *> --- Title (required! - please enforce user to enter) ---
           MOVE 0 TO WS-INPUT-VALID
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Job Title: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF
               MOVE INPUT-RECORD TO WS-TEMP-JOB-TITLE
               IF FUNCTION TRIM(WS-TEMP-JOB-TITLE) = SPACES
                   MOVE "Job Title is required. Please try again."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   MOVE WS-TEMP-JOB-TITLE TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 1 TO WS-INPUT-VALID
               END-IF
           END-PERFORM
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF

      *> --- Description (required info) ---
           MOVE 0 TO WS-INPUT-VALID
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Description (max 200 chars): "
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF
               MOVE INPUT-RECORD TO WS-TEMP-JOB-DESC
               IF FUNCTION TRIM(WS-TEMP-JOB-DESC) = SPACES
                   MOVE "Description is required. Please try again."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   MOVE WS-TEMP-JOB-DESC TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 1 TO WS-INPUT-VALID
               END-IF
           END-PERFORM
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF

      *> --- Employer (required info) ---
           MOVE 0 TO WS-INPUT-VALID
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Employer Name: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF
               MOVE INPUT-RECORD TO WS-TEMP-JOB-EMPLOYER
               IF FUNCTION TRIM(WS-TEMP-JOB-EMPLOYER) = SPACES
                   MOVE "Employer Name is required. Please try again."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   MOVE WS-TEMP-JOB-EMPLOYER TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 1 TO WS-INPUT-VALID
               END-IF
           END-PERFORM
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF

      *> --- Location (required field) ---
           MOVE 0 TO WS-INPUT-VALID
           PERFORM UNTIL WS-INPUT-VALID = 1
               MOVE "Enter Location: " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF
               MOVE INPUT-RECORD TO WS-TEMP-JOB-LOCATION
               IF FUNCTION TRIM(WS-TEMP-JOB-LOCATION) = SPACES
                   MOVE "Location is required. Please try again."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
               ELSE
                   MOVE WS-TEMP-JOB-LOCATION TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 1 TO WS-INPUT-VALID
               END-IF
           END-PERFORM
           IF WS-EOF-FLAG = 1
               EXIT PARAGRAPH
           END-IF

      *> --- Salary (optional, up to user) ---
           MOVE "Enter Salary (optional, enter 'NONE' to skip): "
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF
           MOVE INPUT-RECORD TO WS-TEMP-JOB-SALARY
           MOVE WS-TEMP-JOB-SALARY TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           IF FUNCTION TRIM(WS-TEMP-JOB-SALARY) = "NONE"
               MOVE SPACES TO WS-TEMP-JOB-SALARY
           END-IF

      *> --- Assign tentative ID, then populate record ---
           MOVE WS-JOB-ID-COUNTER                TO JOB-ID
           ADD 1 TO JOB-ID
           MOVE WS-USERNAME(WS-CURRENT-USER-INDEX) TO JOB-POSTER
           MOVE WS-TEMP-JOB-TITLE               TO JOB-TITLE
           MOVE WS-TEMP-JOB-DESC                TO JOB-DESCRIPTION
           MOVE WS-TEMP-JOB-EMPLOYER            TO JOB-EMPLOYER
           MOVE WS-TEMP-JOB-LOCATION            TO JOB-LOCATION
           MOVE WS-TEMP-JOB-SALARY              TO JOB-SALARY
           PERFORM 5330-WRITE-JOB-TO-FILE
           IF WS-JOB-WRITE-SUCCESS = 1
               MOVE JOB-ID TO WS-JOB-ID-COUNTER
               IF WS-JOB-COUNT < WS-MAX-JOBS
                   ADD 1 TO WS-JOB-COUNT
                   MOVE JOB-ID          TO WS-JT-ID(WS-JOB-COUNT)
                   MOVE JOB-POSTER      TO WS-JT-POSTER(WS-JOB-COUNT)
                   MOVE JOB-TITLE       TO WS-JT-TITLE(WS-JOB-COUNT)
                   MOVE JOB-DESCRIPTION TO WS-JT-DESC(WS-JOB-COUNT)
                   MOVE JOB-EMPLOYER    TO WS-JT-EMPLOYER(WS-JOB-COUNT)
                   MOVE JOB-LOCATION    TO WS-JT-LOCATION(WS-JOB-COUNT)
                   MOVE JOB-SALARY      TO WS-JT-SALARY(WS-JOB-COUNT)
               END-IF

      *> --- Confirmation ---
               MOVE "Job posted successfully!" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "----------------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE "Job could not be posted. Please try again."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           EXIT.

       COPY BROWSEJOBS_SRC.

