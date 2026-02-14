      *>*****************************************************************
      *> SENDREQ_SRC - Send Connection Request Sub-Menu (Scaffold)
      *> All output must go through 8000-WRITE-OUTPUT
      *> All input reads must use 8100-READ-INPUT (INPUT.TXT)
      *>*****************************************************************


      *> Entry point paragraph you will PERFORM from main.cob
       7600-SEND-REQUEST-MENU.
           PERFORM 7610-DISPLAY-SENDREQ-MENU
           PERFORM 7620-READ-SENDREQ-CHOICE

           EVALUATE WS-SENDREQ-CHOICE
               WHEN "1"
                   PERFORM 7630-SEND-REQUEST-LOGIC
               WHEN "2"
                   *> Back to main menu: just return to caller
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice. Please try again." TO WS-OUTPUT-LINE
                   PERFORM 8000-WRITE-OUTPUT
                   PERFORM 7600-SEND-REQUEST-MENU
           END-EVALUATE.
           EXIT.

       7610-DISPLAY-SENDREQ-MENU.
           MOVE " " TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "=== SEND CONNECTION REQUEST ===" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "1. Send Connection Request" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "2. Back to Main Menu" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT

           MOVE "Enter your choice:" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT.
           EXIT.

       7620-READ-SENDREQ-CHOICE.
           PERFORM 8100-READ-INPUT
           IF WS-EOF-FLAG = 1
               MOVE 0 TO WS-PROGRAM-RUNNING
               EXIT PARAGRAPH
           END-IF

           MOVE INPUT-RECORD TO WS-SENDREQ-CHOICE
           MOVE WS-SENDREQ-CHOICE TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT.
           EXIT.

      *> Stub for now — real logic будет в следующих задачах
       7630-SEND-REQUEST-LOGIC.
           PERFORM 9300-WRITE-PENDING-REQUEST
           MOVE "Connection request sent!" TO WS-OUTPUT-LINE
           PERFORM 8000-WRITE-OUTPUT
           EXIT.
