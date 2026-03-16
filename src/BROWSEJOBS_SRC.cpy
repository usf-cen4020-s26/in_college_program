      *>*****************************************************************
      *> 5320-BROWSE-JOBS: Display numbered job list, let user select one.
      *> Loops until user enters 0 to go back to job menu.
      *> Uses WS-JOB-TABLE populated at startup by 5355-READ-JOBS-LOOP.
      *>*****************************************************************
       5320-BROWSE-JOBS.
           MOVE 1 TO WS-BROWSE-CHOICE

           PERFORM UNTIL WS-BROWSE-CHOICE = 0
               OR WS-PROGRAM-RUNNING = 0

               MOVE " " TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "--- Available Job Listings ---" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               IF WS-JOB-COUNT = 0
                   MOVE "No jobs are currently available."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE "-----------------------------" TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   MOVE 0 TO WS-BROWSE-CHOICE
                   EXIT PERFORM
               END-IF

               PERFORM VARYING WS-BROWSE-IDX FROM 1 BY 1
                   UNTIL WS-BROWSE-IDX > WS-JOB-COUNT
                   MOVE SPACES TO WS-OUTPUT-LINE
                   MOVE WS-BROWSE-IDX TO WS-DISPLAY-NUM
                   MOVE WS-DISPLAY-NUM TO WS-DISP-ALPHANUM
                   MOVE FUNCTION TRIM(WS-DISP-ALPHANUM) TO WS-NUM-DISP-STR
                   STRING WS-NUM-DISP-STR DELIMITED BY SPACE
                       ". "
                       FUNCTION TRIM(WS-JT-TITLE(WS-BROWSE-IDX))
                       " at "
                       FUNCTION TRIM(WS-JT-EMPLOYER(WS-BROWSE-IDX))
                       " ("
                       FUNCTION TRIM(WS-JT-LOCATION(WS-BROWSE-IDX))
                       ")"
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
               END-PERFORM

               MOVE "-----------------------------" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter job number to view details, or 0 to go back:"
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

               PERFORM 8100-READ-INPUT
               IF WS-EOF-FLAG = 1
                   MOVE 0 TO WS-PROGRAM-RUNNING
                   EXIT PERFORM
               END-IF
               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-MENU-CHOICE
               MOVE WS-JOB-MENU-CHOICE TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT

      *> Validate input is numeric and in range
               MOVE 0 TO WS-BROWSE-CHOICE
               IF FUNCTION TRIM(WS-JOB-MENU-CHOICE) IS NUMERIC
                   MOVE FUNCTION NUMVAL(WS-JOB-MENU-CHOICE)
                       TO WS-BROWSE-CHOICE
               ELSE
                   MOVE 999 TO WS-BROWSE-CHOICE
               END-IF

               EVALUATE TRUE
                   WHEN WS-BROWSE-CHOICE = 0
                       CONTINUE
                   WHEN WS-BROWSE-CHOICE >= 1
                     AND WS-BROWSE-CHOICE <= WS-JOB-COUNT
                       MOVE WS-BROWSE-CHOICE TO WS-SELECTED-JOB-IDX
                       PERFORM 5322-SHOW-JOB-DETAILS
                       MOVE 1 TO WS-BROWSE-CHOICE
                   WHEN OTHER
                       MOVE "Invalid selection. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                       MOVE 999 TO WS-BROWSE-CHOICE
               END-EVALUATE

           END-PERFORM
           MOVE 0 TO WS-BROWSE-CHOICE
           EXIT.

      *>*****************************************************************
      *> 5322-SHOW-JOB-DETAILS: Display full details for selected job.
      *> WS-SELECTED-JOB-IDX must be set before calling.
      *> Offers 1=Apply for this Job, 2=Back to Job List.
      *>*****************************************************************
       5322-SHOW-JOB-DETAILS.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "--- Job Details ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Title: "
               FUNCTION TRIM(WS-JT-TITLE(WS-SELECTED-JOB-IDX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Description: "
               FUNCTION TRIM(WS-JT-DESC(WS-SELECTED-JOB-IDX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Employer: "
               FUNCTION TRIM(WS-JT-EMPLOYER(WS-SELECTED-JOB-IDX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Location: "
               FUNCTION TRIM(WS-JT-LOCATION(WS-SELECTED-JOB-IDX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

      *> Only print Salary line if salary is not blank
           IF FUNCTION TRIM(WS-JT-SALARY(WS-SELECTED-JOB-IDX))
               NOT = SPACES
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "Salary: "
                   FUNCTION TRIM(WS-JT-SALARY(WS-SELECTED-JOB-IDX))
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           END-IF

           MOVE "-------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "1. Apply for this Job" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "2. Back to Job List" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "Enter your choice: " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-DETAIL-CHOICE
           MOVE WS-DETAIL-CHOICE TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           EVALUATE WS-DETAIL-CHOICE
               WHEN "1"
                   PERFORM 5325-APPLY-FOR-JOB
               WHEN "2"
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice. Returning to job list."
                       TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE
           EXIT.
