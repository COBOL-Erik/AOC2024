       identification division.
       program-id. AOC2A.

       environment division.
       configuration section.
      *special-names. decimal-point is comma.
       repository. function all intrinsic.
       input-output section.
       file-control.
           select INFIL assign to 'input.txt'
           organization is line sequential
           file status is INPUT-FS.

       data division.
       file section.
       FD INFIL.
       01 INDATA.
          05 filler           pic X(24).

       working-storage section.
       01 A-ARB.
          05 INPUT-FS         pic XX.
          05 A-TALLY          pic S9(6) comp-4 value ZERO.
          05 A-DIFF           pic S9(6) comp-4 value ZERO.
          05 A-ABS            pic S9(6) comp-4 value ZERO.
          05 IX               pic S9(6) comp-4 value ZERO.
          05 ACC              pic S9(6) comp-4 value ZERO.

       01 array.
          05 L1 pic 99.
          05 L2 pic 99.
          05 L3 pic 99.
          05 L4 pic 99.
          05 L5 pic 99.
          05 L6 pic 99.
          05 L7 pic 99.
          05 L8 pic 99.

       01 argh.
          03 ar99 occurs 1 to 8 times depending on A-TALLY.
             05 a99 pic 99.

      *01 brgh.
      *   03 br99 occurs 1 to 8 times depending on A-TALLY.
      *      05 b99 pic 99.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.
          05 FILLER pic X   value ' '.
             88 V-BIG       value 'B'.
             88 V-LESS      value 'L'.

       procedure division.
       A-MAIN section.
           display 'AOC2A' 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF
              display indata
              move zeroes to array argh A-TALLY 
              unstring indata delimited by all ' '    
                  into L1, L2, L3, L4, L5, L6, L7, L8
              tallying A-TALLY 
              end-unstring
              move array to argh
      *       sort ar99 ascending key a99
              display argh
      *       display brgh
              perform varying ix from 2 by 1 until ix > A-TALLY
      *          display ar99(ix)
                 compute A-ABS  = abs(a99(ix) - a99(ix - 1))
                 compute A-DIFF = a99(ix) - a99(ix - 1)
                 display A-DIFF 
                 evaluate true
                 when A-DIFF = 0 exit perform
                 when A-ABS  > 3 exit perform
                 when other
                    if ix = 2 *> Determine direction
                       if A-DIFF > 0
                          set V-BIG  to true
                       else
                          set V-LESS to true
                       end-if
                    else *> Check direction same as previous
                       if A-DIFF < 0 and V-BIG
                          exit perform
                       end-if
                       if A-DIFF > 0 and V-LESS
                          exit perform
                       end-if
                    end-if
                 end-evaluate
              end-perform
              if ix > A-TALLY
                 add 1 to ACC
              end-if
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           close INFIL
           display ACC ' <---'
           goback
           .
