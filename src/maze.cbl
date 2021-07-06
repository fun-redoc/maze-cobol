       replace ==:err-no-solution-found:== by ==1==
               ==:err99:== by ==99==
               ==:maze-size:== by ==30==
               .
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAZE.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO INFILE
                          FILE STATUS IS PROG-STATUS.
           SELECT outfile ASSIGN TO outfile
                          FILE STATUS IS PROG-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
       FD INFILE RECORD CONTAINS 80 CHARACTERS
                 RECORDING MODE IS F.
       01 fd-INFILE-REC PIC X(80).

       FD outfile RECORD CONTAINS 80 CHARACTERS
                  RECORDING MODE IS F.
       01 fd-outfile-REC PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01 PROG-STATUS PIC 99 VALUE 0.

       01 FILE-STATUS PIC XXX VALUE SPACES.
       88 FILE-STATUS-EOF VALUE 'EOF'.

       01 infile-helper.
           10 infile-rec pic x(80).
           10 infile-row redefines infile-rec
               pic x occurs 80 times indexed by iidx.

       01 time-measure.
           05 start-time.
            10 start-time-num pic 9(16).
            10 start-time-discard pic x(5).
           05 end-time.
            10 end-time-num pic 9(16).
            10 ent-time-discard pic x(5).
           05 delta-time pic 9(16).

       01 maze-def.
           05 start-pos.
            10 start-row usage index.
            10 start-col usage index.
      *    05 dest-pos.
      *     10 dest-row USAGE INDEX .
      *     10 dest-col usage index.
           05 maxrow USAGE INDEX.
           05 rows occurs 99 INDEXED BY ridx try-row dest-row.
            10 row pic x(80).
            10 cols redefines row pic x occurs 80 times
                                        indexed by cidx
                                                   try-col
                                                   dest-col.

       01 nodes.
           05 max-nidx usage index.
           05 nodes-tab occurs 9999 INDEXed by nidx
                                               cur-node-idx.
              10 parent-node-idx usage is index.
              10 node-pos.
                 15 maze-row usage is index.
                 15 maze-col usage is index.
              10 action pic x.

       01 actions-def.
           05 max-aidx usage index.
           05 actions-str pic x(4) value 'LRUD'.
           05 actions REDEFINES actions-str
                      pic x occurs 4 times indexed by aidx.

       01 queue-rec.
           05 queue-status pic x.
           88 queue-status-empty value 'E'.
           05 queue-tab occurs 9999 indexed by qidx
                                               left-idx
                                               right-idx
                                               last-removed.
              10 queue-node-idx usage is index.

       01 search-state-rec.
           05 search-state pic x.
           88 search-state-no-solution value 'X'.
           88 search-state-solution-found value 'F'.
           88 search-state-searching value 'S'.

       01 try-directions.
           05 try-action pic x.
           88 try-left value 'L'.
           88 try-right value 'R'.
           88 try-up value 'U'.
           88 try-down value 'D'.

       LINKAGE SECTION.
       01  PARM-BUFFER.
          05  PARM-LENGTH         pic S9(4) comp.
          05  PARM-DATA           pic X(256).
       PROCEDURE DIVISION using PARM-BUFFER.
      *----------------------------------------------------------------
       DECLARATIVES.
       DEBUG SECTION.
             USE FOR DEBUGGING ON ALL PROCEDURES.
       DEBUG-DECLARATIVES-PARAGRAPH.
             DISPLAY '>> ', DEBUG-ITEM.
       END DECLARATIVES.
      *----------------------------------------------------------------
       A000-Main section.
           display "-------------"
           display "----START----"
           display "-------------"

           move function CURRENT-DATE to start-time
           *> do something important
           open input INFILE
           if PROG-STATUS = 0
              open output outfile
              if PROG-STATUS = 0
                 perform a100-read-maze
                 perform a200-maze-search
                 if search-state-no-solution
                    display "no solution found"
                    move :err-no-solution-found: to PROG-STATUS
                 end-if
                 if search-state-solution-found
                    display "solution exists"
                    perform a300-out-result
                 end-if
                 close outfile
              END-IF
              close infile
           END-IF

           *> don something important
           move function CURRENT-DATE to end-time
           compute delta-time = end-time-num - start-time-num
           display "duration: " delta-time

           DISPLAY '#PROGRAM maze ENDS WITH STATUS ' PROG-STATUS
           MOVE PROG-STATUS TO RETURN-CODE

           display "-----------"
           display "----END----"
           display "-----------"

           goback.

       a100-read-maze section.

           INITIALIZE FILE-STATUS
           set ridx to 1
           perform until FILE-STATUS-EOF
              read infile into INFILE-REC
               at end set FILE-STATUS-EOF to true
                 SET MAXROW TO RIDX
               not at END
                 *> initialize row with walls
                 PERFORM VARYING cidx
                  FROM 1 BY 1
                  UNTIL cidx > 80
                  move 'x' to cols(ridx, cidx)
                 end-perform

                 move INFILE-REC to row(RIDX)
                 *> check start
                 set iidx to 1
                 search infile-row
                   when infile-row(iidx) = 'A'
                        set start-row to ridx
                        set start-col to iidx
                  when infile-row(iidx) = 'B'
                       set dest-row to ridx
                       set dest-col to iidx
                 end-search
                 set RIDX up by 1
              END-READ
           end-perform

           continue.


       a200-maze-search section.

           perform b100-initialize-frontier
           perform c100-initialize-search
           perform c200-search-alg

           CONTINUE.

       a300-out-result section.

           *> fill maze with the way
           set nidx to parent-node-idx(cur-node-idx) *> cur node points to B
                                                     *> B should stay in the out
           PERFORM test after
                   until parent-node-idx(nidx) = nidx
               set ridx to maze-row(nidx)
               set cidx to maze-col(nidx)
               move '.' to cols(ridx, cidx)
               set nidx to parent-node-idx(nidx)
           END-PERFORM
           *> uncommenting those lines leads to overwrite of A
           *> A should remain in the output
      *    set ridx to maze-row(nidx)
      *    set cidx to maze-col(nidx)
      *    move '.' to cols(ridx, cidx)

           perform test before
                   varying ridx from 1 by 1
                   until ridx = maxrow

      D     display "DEBUG> " row(ridx)
      *     move row(nidx) to fd-outFILE-REC
            WRITE fd-outfile-REC from row(ridx)

           end-perform

           continue.

       b100-initialize-frontier section.

           set qidx to 1
           set left-idx  to qidx
           set right-idx to qidx

           CONTINUE .

       b200-queue-check-empty section.
           *> if queue emtpy sets the queue-status-empty to true
           *> otherwise does nothing
           if RIGHT-IDX = left-idx
      D      display "DEBUG> queue empty"
             set queue-status-empty to true
           end-if
           continue.

       b300-remove-node-from-queue SECTIOn.
           set last-removed to left-idx
           set left-idx up by 1
           continue.

       c100-initialize-search section.

           set nidx to 1
           set max-nidx to nidx
           set search-state-searching to true
           *> enququ initial node
           set maze-row(nidx) to start-row
           set maze-col(nidx) to start-col
           set queue-node-idx(right-idx) to nidx
           set parent-node-idx(nidx) to nidx *> parent = self means initial node
           set right-idx up by 1

           CONTINUE .

       c110-check-goal-state section.
           set ridx to maze-row(cur-node-idx)
           set cidx to maze-col(cur-node-idx)

           display "DEBUG> Cur  Field: '"
                   cols(ridx, cidx)
                   "' Dest Field: '"
                   cols(dest-row, dest-col)
                   "'"

           IF maze-row(cur-node-idx) = dest-row
              and maze-col(cur-node-idx) = dest-col
           then
             set search-state-solution-found to true
           END-IF

           continue.

       C120-find-and-enq-next-moves section.

           set aidx to 4
           set max-aidx to aidx
           PERFORM c121-try-direction VARYING aidx
            FROM 1 BY 1
            UNTIL aidx > max-aidx

           CONTINUE.

        c121-try-direction SECTION.
           move actions(aidx) to try-action
           set try-row to maze-row(cur-node-idx)
           set try-col to maze-col(cur-node-idx)
           EVALUATE true
               WHEN try-right
                  set try-col up by 1
               WHEN try-left
                  set try-col down by 1
               WHEN try-up
                  set try-row up by 1
               WHEN try-down
                  set try-row down by 1
               WHEN OTHER
      D           DISPLAY "DEBUG> Program Error."
                  CONTINUE
           END-EVALUATE

           if maxrow > try-row
           and not cols(try-row, try-col) = 'x'
           then
              *> TODO
              *> 1. check if not yet a node
              *> 2. enqueue
              PERFORM VARYING nidx
               FROM 1 BY 1
               UNTIL nidx > max-nidx
               or maze-row(nidx) = try-row
               and maze-col(nidx) = try-col
              end-perform
              if nidx > max-nidx
              then *> node not yet examined
                 *> so enqueue
      D          display "DEBUG> enqueue " try-action
                                           cols(try-row, try-col)
                 set max-nidx to nidx
                 set maze-row(nidx) to try-row
                 set maze-col(nidx) to try-col
                 move try-action to action(nidx)
                 set parent-node-idx(nidx) to cur-node-idx
                 set queue-node-idx(right-idx) to nidx
                 set right-idx up by 1
              end-if
           end-if

           continue.


       c200-search-alg section.
           perform until search-state-no-solution
                      or search-state-solution-found
            perform b200-queue-check-empty
            if queue-status-empty
              set search-state-no-solution to true
            else
              perform b300-remove-node-from-queue
              set cur-node-idx to last-removed
              perform c110-check-goal-state
              if NOT search-state-solution-found
                PERFORM C120-find-and-enq-next-moves
              END-IF
            end-if
           end-perform
           CONTINUE .
