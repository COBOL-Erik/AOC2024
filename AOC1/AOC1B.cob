       identification division.
       program-id. AOC1B.

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
       FD  INFIL.
       01  INDATA.
           05  FLI1           pic 9(5).
           05  FILLER         pic X(3).
           05  FLI2           pic 9(5).

       working-storage section.
       01 A-ARB.
          05 INPUT-FS         pic XX.
          05 A-COUNT          pic S9(6) comp-4 value ZERO.
          05 IX               pic S9(6) comp-4 value ZERO.
          05 IX2              pic S9(6) comp-4 value ZERO.
          05 IX3              pic S9(6) comp-4 value ZERO.
          05 ACC              pic S9(8) comp-4 value ZERO.
          05 multiplier       pic S9(8) comp-4 value ZERO.

       01 FILLER.
          05 LIST1 occurs 1 to 5000 times depending on A-COUNT.
             10 L1            pic 9(5) value HIGH-VALUES.
       01 FILLER.
          05 LIST2 occurs 1 to 5000 times depending on A-COUNT.
             10 L2            pic 9(5) value HIGH-VALUES.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.

       procedure division.
       A-MAIN section.
           display 'AOC1B' 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF
              add 1 to A-COUNT
              move FLI1 to LIST1(A-COUNT)
              move FLI2 to LIST2(A-COUNT)
      *       display LI1 ' www ' LI2  
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           close INFIL
           sort LIST1 ascending key L1
           sort LIST2 ascending key L2
           perform varying ix from 1 by 1 until ix > A-COUNT
              move ZERO to multiplier
              perform varying ix2 from 1   by 1 until ix2 > A-COUNT
                                                   or L1(ix) = L2(ix2)
                 continue
              end-perform
              perform varying ix3 from ix2 by 1 until ix3 > A-COUNT
                                                   or L1(ix) < L2(ix3)
                 add 1 to multiplier 
              end-perform
              compute ACC = ACC + L1(ix) * multiplier
           end-perform
           display ACC
           goback
           .
