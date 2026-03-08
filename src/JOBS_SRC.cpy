*> *      *>*****************************************************************
*> *      *> 5300-JOB-SEARCH-MENU: Job Search/Internship submenu loop
*> *      *> Displays submenu and handles user input for job search/internship options
*> *      *>*****************************************************************
       5300-JOB-SEARCH-MENU.
           MOVE "1" TO WS-JOB-MENU-CHOICE

           PERFORM UNTIL WS-JOB-MENU-CHOICE = "3"
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
               MOVE "3. Back to Main Menu" TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
               MOVE "Enter your choice (1-3): " TO WS-OUTPUT-LINE
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
                       PERFORM 5310-POST-JOB-STUB
                   WHEN "2"
                       PERFORM 5320-BROWSE-JOBS-STUB
                   WHEN "3"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again."
                           TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
               END-EVALUATE

           END-PERFORM.
           EXIT.

*> *      *>*****************************************************************
*> *      *> 5310-POST-JOB-STUB: Post a Job/Internship placeholder                   *
*> *      *> Submenu is empty (will be implemented soon)                             *
*> *      *>*****************************************************************
       5310-POST-JOB-STUB.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "--- Post a New Job/Internship ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "Post a Job/Internship is under construction."
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

*> *      *>*****************************************************************
*> *      *> 5320-BROWSE-JOBS-STUB: Browse Jobs/Internships placeholder                   *
*> *      *> Remains under construction per Epic 6 spec.                                  *
*> *      *>*****************************************************************
       5320-BROWSE-JOBS-STUB.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "Browse Jobs/Internships is under construction."
               TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.