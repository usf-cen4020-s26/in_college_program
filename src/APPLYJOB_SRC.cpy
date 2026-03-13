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
