>>SOURCE FORMAT FREE
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
                   STRING WS-BROWSE-IDX ". "
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
               IF FUNCTION TRIM(WS-JOB-MENU-CHOICE) = SPACES
                   MOVE 999 TO WS-BROWSE-CHOICE
               ELSE
                   IF FUNCTION TRIM(WS-JOB-MENU-CHOICE) IS NUMERIC
                       MOVE FUNCTION NUMVAL(WS-JOB-MENU-CHOICE)
                           TO WS-BROWSE-CHOICE
                   ELSE
                       MOVE 999 TO WS-BROWSE-CHOICE
                   END-IF
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

           IF WS-DETAIL-CHOICE = "1"
               PERFORM 5325-APPLY-FOR-JOB
           END-IF

           IF WS-DETAIL-CHOICE NOT = "1" AND WS-DETAIL-CHOICE NOT = "2"
               MOVE "Invalid choice. Returning to job list."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           EXIT.
      *>*****************************************************************
      *> 5325-APPLY-FOR-JOB: Check for duplicate application, then record.
      *> Prerequisite: WS-SELECTED-JOB-IDX must be set before calling.
      *>*****************************************************************
       5325-APPLY-FOR-JOB.
           MOVE 0 TO WS-APP-FOUND
           PERFORM 5327-CHECK-DUPLICATE-APPLICATION
           IF WS-APP-FOUND = 1
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "You have already applied for "
                   FUNCTION TRIM(WS-JT-TITLE(WS-SELECTED-JOB-IDX))
                   " at "
                   FUNCTION TRIM(WS-JT-EMPLOYER(WS-SELECTED-JOB-IDX))
                   "."
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM 5326-WRITE-APPLICATION
           IF WS-APPS-WRITE-SUCCESS = 1
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "Your application for "
                   FUNCTION TRIM(WS-JT-TITLE(WS-SELECTED-JOB-IDX))
                   " at "
                   FUNCTION TRIM(WS-JT-EMPLOYER(WS-SELECTED-JOB-IDX))
                   " has been submitted."
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               ADD 1 TO WS-APP-COUNT
           ELSE
               MOVE "Application could not be saved. Please try again."
                   TO WS-OUTPUT-LINE
               PERFORM 8000-WRITE-OUTPUT
           END-IF
           EXIT.

      *>*****************************************************************
      *> 5327-CHECK-DUPLICATE-APPLICATION: Scan APPLICATIONS.DAT for
      *> a record matching current user + selected job ID.
      *> Sets WS-APP-FOUND = 1 if duplicate found, 0 otherwise.
      *>*****************************************************************
       5327-CHECK-DUPLICATE-APPLICATION.
           MOVE 0   TO WS-APP-FOUND
           MOVE "N" TO WS-APPS-EOF
           OPEN INPUT APPLICATIONS-FILE

           EVALUATE WS-APPS-STATUS
               WHEN "00"
                   CONTINUE
               WHEN "35"
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open APPLICATIONS.DAT. STATUS="
                       WS-APPS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
                   EXIT PARAGRAPH
           END-EVALUATE

           PERFORM UNTIL WS-APPS-EOF = "Y" OR WS-APP-FOUND = 1
               READ APPLICATIONS-FILE
                   AT END
                       MOVE "Y" TO WS-APPS-EOF
                   NOT AT END
                       IF FUNCTION TRIM(APP-USERNAME)
                            = FUNCTION TRIM(
                                WS-USERNAME(WS-CURRENT-USER-INDEX))
                         AND APP-JOB-ID
                              = WS-JT-ID(WS-SELECTED-JOB-IDX)
                           MOVE 1 TO WS-APP-FOUND
                       END-IF
               END-READ
           END-PERFORM

           CLOSE APPLICATIONS-FILE
           EXIT.

      *>*****************************************************************
      *> 5326-WRITE-APPLICATION: Append one application record to
      *> APPLICATIONS.DAT. Sets WS-APPS-WRITE-SUCCESS = 1 on success.
      *>*****************************************************************
       5326-WRITE-APPLICATION.
           MOVE 0 TO WS-APPS-WRITE-SUCCESS

           MOVE WS-USERNAME(WS-CURRENT-USER-INDEX) TO APP-USERNAME
           MOVE WS-JT-ID(WS-SELECTED-JOB-IDX)       TO APP-JOB-ID
           MOVE WS-JT-TITLE(WS-SELECTED-JOB-IDX)    TO APP-JOB-TITLE
           MOVE WS-JT-EMPLOYER(WS-SELECTED-JOB-IDX) TO APP-JOB-EMPLOYER
           MOVE WS-JT-LOCATION(WS-SELECTED-JOB-IDX) TO APP-JOB-LOCATION

           OPEN EXTEND APPLICATIONS-FILE

           IF WS-APPS-STATUS = "35"
               OPEN OUTPUT APPLICATIONS-FILE
               CLOSE APPLICATIONS-FILE
               OPEN EXTEND APPLICATIONS-FILE
           END-IF

           IF WS-APPS-STATUS NOT = "00"
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not open APPLICATIONS.DAT. STATUS="
                   WS-APPS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           WRITE APP-RECORD

           IF WS-APPS-STATUS NOT = "00"
               MOVE SPACES TO WS-OUTPUT-LINE
               STRING "ERROR: Could not write to APPLICATIONS.DAT. STATUS="
                   WS-APPS-STATUS
                   DELIMITED BY SIZE INTO WS-OUTPUT-LINE
               END-STRING
               PERFORM 8000-WRITE-OUTPUT
           ELSE
               MOVE 1 TO WS-APPS-WRITE-SUCCESS
           END-IF

           CLOSE APPLICATIONS-FILE
           EXIT.
      *>*****************************************************************
      *> 5340-VIEW-MY-APPLICATIONS: Generate job application summary report
      *> for the currently logged-in user.
      *> Reads APPLICATIONS.DAT, filters by username, prints report.
      *>*****************************************************************
       5340-VIEW-MY-APPLICATIONS.
           MOVE 0   TO WS-APP-COUNT
           MOVE "N" TO WS-APPS-EOF

           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           MOVE "--- Your Job Applications ---" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Application Summary for "
               FUNCTION TRIM(WS-USERNAME(WS-CURRENT-USER-INDEX))
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE "------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           OPEN INPUT APPLICATIONS-FILE

           EVALUATE WS-APPS-STATUS
               WHEN "00"
                   PERFORM 5341-READ-APPS-LOOP
                   CLOSE APPLICATIONS-FILE
               WHEN "35"
                   CONTINUE
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open APPLICATIONS.DAT. STATUS="
                       WS-APPS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE

           MOVE "------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE SPACES TO WS-OUTPUT-LINE
           STRING "Total Applications: "
               WS-APP-COUNT
               DELIMITED BY SIZE INTO WS-OUTPUT-LINE
           END-STRING
           PERFORM 8000-WRITE-OUTPUT

           MOVE "------------------------------" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.

      *>*****************************************************************
      *> 5341-READ-APPS-LOOP: Recursively read APPLICATIONS.DAT,
      *> printing records belonging to the current logged-in user.
      *>*****************************************************************
       5341-READ-APPS-LOOP.
           READ APPLICATIONS-FILE
               AT END
                   MOVE "Y" TO WS-APPS-EOF
               NOT AT END
                   IF FUNCTION TRIM(APP-USERNAME)
                        = FUNCTION TRIM(
                            WS-USERNAME(WS-CURRENT-USER-INDEX))
                       ADD 1 TO WS-APP-COUNT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Job Title: "
                           FUNCTION TRIM(APP-JOB-TITLE)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Employer: "
                           FUNCTION TRIM(APP-JOB-EMPLOYER)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE SPACES TO WS-OUTPUT-LINE
                       STRING "Location: "
                           FUNCTION TRIM(APP-JOB-LOCATION)
                           DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                       END-STRING
                       PERFORM 8000-WRITE-OUTPUT

                       MOVE "---" TO WS-OUTPUT-LINE
                       PERFORM 8000-WRITE-OUTPUT
                   END-IF
           END-READ

           IF WS-APPS-EOF = "N"
               PERFORM 5341-READ-APPS-LOOP
           END-IF
           EXIT.

      *>*****************************************************************
      *> 5360-LOAD-APPLICATIONS: Startup check for APPLICATIONS.DAT.
      *> Verifies file accessibility; warns on unexpected status codes.
      *>*****************************************************************
       5360-LOAD-APPLICATIONS.
           MOVE "N" TO WS-APPS-EOF

           OPEN INPUT APPLICATIONS-FILE

           EVALUATE WS-APPS-STATUS
               WHEN "00"
                   CLOSE APPLICATIONS-FILE
               WHEN "35"
                   CONTINUE
               WHEN OTHER
                   MOVE SPACES TO WS-OUTPUT-LINE
                   STRING "WARNING: Could not open APPLICATIONS.DAT. STATUS="
                       WS-APPS-STATUS
                       DELIMITED BY SIZE INTO WS-OUTPUT-LINE
                   END-STRING
                   PERFORM 8000-WRITE-OUTPUT
           END-EVALUATE
           EXIT.
