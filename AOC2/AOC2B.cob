       identification division.
       program-id. AOC2B.

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
          05 A-COUNT          pic S9(6) comp-4 value ZERO.
          05 A-TALLY          pic S9(6) comp-4 value ZERO.
          05 A-DIFF           pic S9(6) comp-4 value ZERO.
          05 A-ABS            pic S9(6) comp-4 value ZERO.
          05 IX               pic S9(6) comp-4 value ZERO.
          05 SX               pic S9(6) comp-4 value ZERO.
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

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.
          05 FILLER pic X   value ' '.
             88 V-BIG       value 'B'.
             88 V-LESS      value 'L'.
          05 FILLER pic X   value 'W'.
             88 V-DONE      value 'D'.
             88 V-WORK      value 'W'.
          05 FILLER pic X   value 'Q'.
             88 V-FIRST     value 'F' false 'Q'.

       procedure division.
       A-MAIN section.
           display 'AOC2B' 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF
              add 1 to A-COUNT 
              move zeroes to array argh A-TALLY 
              unstring indata delimited by all ' '    
                  into L1, L2, L3, L4, L5, L6, L7, L8
              tallying A-TALLY 
              end-unstring
              move array to argh
              set V-WORK to true
              set V-FIRST to false
              perform varying sx from 0 by 1 until sx > A-TALLY
                                                or V-DONE
                 display ' ' WITH NO ADVANCING 
                 perform grind 
              end-perform
              display ' '
              if V-DONE
                 add 1 to ACC
              else
                 display A-COUNT ':' argh ' ' sx ' ' ix
              end-if
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           close INFIL
           display ACC ' <---'
           goback
           .

       grind section.
           perform varying ix from 2 by 1 until ix > A-TALLY
      *       display 'a99(ix) sx ix ' a99(ix) ' ' sx ' ' ix
              evaluate true
              when sx = ix and ix = 2
                 display '!' with no advancing
                 compute A-ABS  = abs(a99(ix + 1) - a99(ix - 1))
                 compute A-DIFF = a99(ix + 1) - a99(ix - 1)
              when sx = ix
                 display '=' with no advancing
                 exit perform cycle
              when sx = ix - 1
                 if ix = 2 *> sx = 1
                    set V-FIRST to true *> Check direction next cycle
                    display 'f' WITH NO ADVANCING 
                    exit perform cycle
                 end-if
                 compute A-ABS  = abs(a99(ix) - a99(ix - 2))
                 compute A-DIFF = a99(ix) - a99(ix - 2)
              when other
                 compute A-ABS  = abs(a99(ix) - a99(ix - 1))
                 compute A-DIFF = a99(ix) - a99(ix - 1)
              end-evaluate
              evaluate true
              when A-DIFF = 0
                 display 'd' WITH NO ADVANCING 
                 exit perform
              when A-ABS  > 3
                 display 'a' WITH NO ADVANCING 
                 exit perform
              when other *> So far so good...
                 if ix = 2 or V-FIRST *> Determine direction
                    if A-DIFF > 0
                       set V-BIG  to true
                       display 'b' WITH NO ADVANCING
                    else
                       display 'l' WITH NO ADVANCING
                       set V-LESS to true
                    end-if
                    set V-FIRST   to false
                    display 'o' WITH NO ADVANCING 
                 else *> Check direction same as previous
                    if A-DIFF < 0 and V-BIG
                       display '<' WITH NO ADVANCING 
                       exit perform
                    end-if
                    if A-DIFF > 0 and V-LESS
                       display '>' WITH NO ADVANCING 
                       exit perform
                    end-if
                 end-if
                 display 'w' WITH NO ADVANCING 
              end-evaluate
           end-perform
           if ix > A-TALLY
              set V-DONE to true
           end-if
           .
