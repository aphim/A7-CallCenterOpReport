       identification division.
       program-id. A7-CallCenterOpReport.
       author. Jacky Yuan.
       date-written. 2021-04-02.
      *Program Description:
      *    Program to create Call Center Operator Report.
      *
       environment division.
       input-output section.
       file-control.
      *
           select input-file
               assign to '../../../../data/A7.dat'
               organization is line sequential.
      *
           select report-file
               assign to '../../../../data/A7-CallCenterOpReport.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd input-file
           data record is emp-rec.
      *
       01 emp-rec.
         05 emp-rec-num                pic x(3).
         05 emp-rec-name               pic x(12).
         05 emp-rec-calls              pic 9(3) occurs 12 times.
      *
       fd report-file
           data record is print-line.
      *
       01 print-line                   pic x(132).
      *
       working-storage section.
      *
       01 ws-constants.
         05 ws-number-of-months        pic 99 value 12.
         05 ws-month-names             pic x(36) value
                               "JULAUGSEPOCTNOVDECJANFEBMARAPRMAYJUN".
         05 ws-month-literals redefines ws-month-names pic x(3) occurs
                              12 times.
      *
      *File titles and headers
       01 ws-name-line.
         05 filler                     pic x(85) value spaces.
         05 filler                     pic x(28) value 
                                       '  Jacky Yuan, Assignment 7'.
         05 filler                     pic x(18) value spaces.
      *
       01 ws-report-heading.
         05 filler                     pic x(37) value spaces.
         05 filler                     pic x(40) value
                            '  Call Centre Volumes for  July - June  '.
         05 filler                     pic x(55) value spaces.
      *
       01 ws-heading-line1.
         05 filler                     pic x(40) value
                            '  Operator  Operator      Jul   Aug   Se'.
         05 filler                     pic x(40) value
                            'p   Oct   Nov   Dec   Jan   Feb   Mar   '.
         05 filler                     pic x(40) value
                            'Apr   May   Jun    Total   Avg REM      '.
         05 filler                     pic x(12) value spaces.
      *
       01 ws-heading-line2.
         05 filler                     pic x(40) value
                            "     #       Name                       ".
         05 filler                     pic x(40) value
                            "                                        ".
         05 filler                     pic x(52) value spaces.
      *
      *Data record entries
       01 ws-detail-line.
         05 filler                     pic x(4) value spaces.
         05 ws-dl-num                  pic x(3) value spaces.
         05 filler                     pic x(6) value spaces.
         05 ws-dl-name                 pic x(12) value spaces.
         05 filler                     pic x(1) value spaces.
         05 ws-dl-months-table occurs 12 times.
           10 ws-dl-months             pic zz9.
           10 ws-dl-filler             pic x(3).
         05 filler                     pic x(2) value spaces.
         05 ws-dl-total                pic zzz9 value 0.
         05 filler                     pic x(2) value spaces.
         05 ws-dl-avg                  pic zzz9 value 0.
         05 ws-dl-avg-text redefines ws-dl-avg pic x(4).
         05 filler                     pic x(2) value spaces.
         05 ws-dl-rem                  pic z9 value 0.
         05 ws-dl-rem-text redefines ws-dl-rem pic xx.
         05 filler                     pic x(18) value spaces.
      *
      *Summary lines
       01 ws-ops-line.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(10) value "Operators ".
         05 filler                     pic x(10) value "with calls".
         05 ws-ol-months-table occurs 12 times.
           10 ws-ol-months             pic zzzz9 value 0.
           10 ws-ol-filler             pic x(1).
         05 filler                     pic x(36) value spaces.
      *
       01 ws-total-line.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(10) value "Totals    ".
         05 filler                     pic x(10) value spaces.
         05 ws-tl-months-table occurs 12 times.
           10 ws-tl-months             pic zzzz9 value 0.
           10 ws-tl-filler             pic x(1).
         05 filler                     pic x(3) value spaces.
         05 ws-tl-total                pic zzzz9 value 0.
         05 filler                     pic x(1) value spaces.
         05 ws-tl-avg                  pic zzzz9 value 0.
         05 filler                     pic x(1) value spaces.
         05 ws-tl-rem                  pic zz9 value 0.
         05 filler                     pic x(18) value spaces.
      *
       01 ws-average-line.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(10) value "Averages  ".
         05 filler                     pic x(10) value spaces.
         05 ws-al-months-table occurs 12 times.
           10 ws-al-months             pic zzzz9 value 0.
           10 ws-al-filler             pic x(1).
         05 filler                     pic x(36) value spaces.
      *
       01 ws-total-line-no-calls.
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(38) value
                              'Number of Operators with No Calls:    '.
         05 filler                     pic x(21) value spaces.
         05 ws-tl-zero-calls           pic z9 value 0.
         05 filler                     pic x(37) value spaces.
         05 filler                     pic x(52) value spaces.
      *
       01 ws-total-line-no-calls-months.
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(23) value 
                                       'Number of Months where '.
         05 filler                     pic x(28) value 
                                       'Operators have No Calls:    '.
         05 filler                     pic x(8) value spaces.
         05 ws-tl-zero-mths            pic z9 value 0.
         05 filler                     pic x(89) value spaces.
      *
       01 ws-total-line-highest-month-average.
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(23) value 
                                       'Month with the Highest '.
         05 filler                     pic x(26) value 
                                       'Monthly Average:          '.
         05 ws-tl-h-mth-avg            pic zzzz9 value 0.
         05 filler                     pic x(4) value spaces.
         05 ws-tl-h-mth-nam            pic x(3) value "XXX".
         05 filler                     pic x(66) value spaces.
      *
       01 ws-tl-highest-operator-average.
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(23) value 
                                       'Operator with the Highe'.
         05 filler                     pic x(28) value 
                                       'st Monthly Average:         '.
         05 ws-tl-h-op-num             pic x(3) value "XXX".
         05 filler                     pic x(2) value spaces.
         05 ws-tl-h-op-avg             pic zzzz9 value 0.
         05 filler                     pic x(68) value spaces.
      *
       01 ws-tl-lowest-operator-average.
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(23) value 
                                       'Operator with the Lowes'.
         05 filler                     pic x(28) value 
                                       't Monthly Average:          '.
         05 ws-tl-l-op-num             pic x(3) value "XXX".
         05 filler                     pic x(2) value spaces.
         05 ws-tl-l-op-avg             pic zzzz9 value 0.
         05 filler                     pic x(68) value spaces.
      *
       01 ws-total-line-overall.
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(20) value 
                                       "Overall Total Calls:".
         05 filler                     pic x(36) value spaces.
         05 ws-tl-all-calls            pic zzzz9 value 0.
      *
      *Array of operators with calls and their total calls
       01 ws-calls-array.
         05 ws-records occurs 12 times.
           10 ws-ops-with-calls        pic 999.
           10 ws-ops-totals            pic 9(5).
      *
      *Array of monthly average calls
       01 ws-avgs-array.
         05 ws-records occurs 12 times.
           10 ws-monthly-avg           pic 999.
      *
      *Calculation variables
       01 ws-calc.
         05 ws-operator-sum            pic 9(5) value 0.
         05 ws-operator-avg-calc       pic 999 value 0.
         05 ws-avg-remainder           pic 999 value 0.
         05 ws-overall-total           pic 9(6) value 0.
         05 ws-overall-calls           pic 9(6) value 0.
         05 ws-overall-avg             pic 9(6) value 0.
         05 ws-overall-avg-rem         pic 999 value 0.
         05 ws-current-high-month      pic 9999 value 0.
         05 ws-high-op                 pic 999 value 0.
         05 ws-low-op                  pic 9(5) value 10000.
      *
      *Counter variables
       01 ws-counters.
         05 ws-operator-call-count     pic 999 value 0.
         05 ws-no-calls                pic 99 value 0.
         05 ws-no-call-month           pic 99 value 0.
      *
      *Constants
       77 ws-zero                      pic 9 value 0.
       77 ws-one                       pic 9 value 1.
       77 ws-zero-avg                  pic xxxx value "ZERO".
       77 ws-eof-flag                  pic x value "N".
       77 ws-sub                       pic 99 value 1.
       77 ws-file-empty                pic x value "e".
      *
       procedure division.
       000-Main.
      *
           perform 100-open-files.
           perform 200-heading-output.
           perform 300-read-file.
           perform 400-process-lines
             until ws-eof-flag equals ws-file-empty.
           perform 600-summary-block.
           perform 700-output-summary.
           perform 1000-close-files.
      *
      * open files
       100-open-files.
      *
           open input input-file.
           open output report-file.
      *
      * output heading
       200-heading-output.
           write print-line            from ws-name-line.
           write print-line            from ws-report-heading.
           write print-line            from ws-heading-line1.
           write print-line            from ws-heading-line2.
      *
      *Reads through the input file
       300-read-file.
      *
           read input-file
               at end
                   move ws-file-empty  to ws-eof-flag.
      *
      *Processes the data record lines
       400-process-lines.
      *
           perform 420-reset-counters.
           move emp-rec-num            to ws-dl-num.
           move emp-rec-name           to ws-dl-name.
      *Loop that iterates a sub variable to loop through array
           perform varying ws-sub      from ws-one by ws-one
             until ws-sub > ws-number-of-months
      *
               move emp-rec-calls(ws-sub) to ws-dl-months(ws-sub)
               perform 500-process-records
      *
           end-perform.
      *
           perform 540-total-average.
      *
      *Checks for the case of an operator with no calls
           write print-line            from ws-detail-line
           if (ws-operator-sum = ws-zero) then
               add ws-one              to ws-no-calls
           end-if.
      *
      *Checks for operator with the highest average 
           if (ws-operator-avg-calc > ws-high-op) then
               move ws-operator-avg-calc to ws-high-op
               move emp-rec-num        to ws-tl-h-op-num
               move ws-high-op         to ws-tl-h-op-avg
           end-if.
      *
      *Checks for the operator with the lowest average
           if (ws-operator-avg-calc < ws-low-op and
             ws-operator-avg-calc not = ws-zero) then
               move ws-operator-avg-calc to ws-low-op
               move emp-rec-num        to ws-tl-l-op-num
               move ws-low-op          to ws-tl-l-op-avg
           end-if.
           perform 300-read-file.
      *
      *This resets the counters for the next data entry
       420-reset-counters.
      *
           move ws-zero                to ws-operator-sum.
           move ws-zero                to ws-operator-avg-calc.
           move ws-zero                to ws-operator-call-count.
           move ws-zero                to ws-avg-remainder.
           move ws-zero                to ws-dl-rem.
      *
      *This processes the individual record
       500-process-records.
      *
      *Checks if the record is 0 and if not performs the calculations
           if not (emp-rec-calls(ws-sub) equals ws-zero) then
               add emp-rec-calls(ws-sub) to ws-operator-sum
               add ws-one              to ws-operator-call-count
               divide ws-operator-sum by ws-operator-call-count
                 giving ws-operator-avg-calc
                 remainder ws-avg-remainder
               perform 520-monthly-calls
           end-if.
      *
      *Checks if the current month's call is equal to 0
           if (emp-rec-calls(ws-sub) equals ws-zero) then
               add ws-one              to ws-no-call-month
           end-if.
           move ws-operator-sum        to ws-dl-total.
           move ws-operator-avg-calc   to ws-dl-avg.
      *
      *Check if their overall average is equal to 0
           if (ws-operator-avg-calc = ws-zero) then
               move ws-zero-avg        to ws-dl-avg
           end-if.
           move ws-avg-remainder       to ws-dl-rem.

      *
      *Processes the data to form the monthly averages among all the 
      *operators
       520-monthly-calls.
      *
           add ws-one                  to ws-ops-with-calls(ws-sub).
           add emp-rec-calls(ws-sub)   to ws-ops-totals(ws-sub).
           move ws-ops-with-calls(ws-sub) to ws-ol-months(ws-sub).
           move ws-ops-totals(ws-sub)  to ws-tl-months(ws-sub).
      *
           divide ws-ops-totals(ws-sub) by ws-ops-with-calls(ws-sub)
             giving ws-monthly-avg(ws-sub).
      *
           move ws-monthly-avg(ws-sub) to ws-al-months(ws-sub).
      *
      *Sums up the totals and moves them to the output
       540-total-average.
      *
           add ws-operator-sum         to ws-overall-total.
           add ws-operator-avg-calc    to ws-overall-avg.
           add ws-avg-remainder        to ws-overall-avg-rem.
           move ws-overall-total       to ws-tl-total.
           move ws-overall-avg         to ws-tl-avg.
           move ws-overall-avg-rem     to ws-tl-rem.
      *
      *Output total lines in the summary
       600-summary-block.
      *
           move ws-no-calls            to ws-tl-zero-calls.
           move ws-no-call-month       to ws-tl-zero-mths.
           move ws-overall-total       to ws-tl-all-calls.
           perform 650-highest-monthly-avg.
      *
      *loops through the monthly averages to find the highest month
       650-highest-monthly-avg.
      *
           perform varying ws-sub from ws-one by ws-one
             until ws-sub > ws-number-of-months
               if (ws-monthly-avg(ws-sub) > ws-current-high-month) then
                   move ws-sub to ws-current-high-month
                   move ws-month-literals(ws-sub) to ws-tl-h-mth-nam
               end-if
           end-perform.
           move ws-current-high-month  to ws-tl-h-mth-avg.
      *
      *Outputs the summary lines of the program
       700-output-summary.
           write print-line            from ws-ops-line
           after advancing 1 line.
           write print-line            from ws-total-line.
           write print-line            from ws-average-line.
           write print-line            from ws-total-line-no-calls
           after advancing 1 line.
           write print-line            from 
                                       ws-total-line-no-calls-months.
           write print-line            from 
                                       ws-tl-highest-operator-average.
           write print-line            from 
                                       ws-tl-lowest-operator-average.
           write print-line            from
                                   ws-total-line-highest-month-average.
           write print-line            from ws-total-line-overall.
      *
      *close files
       1000-close-files.
      *
           close input-file.
           close report-file.
           stop run.
      *
       end program A7-CallCenterOpReport.