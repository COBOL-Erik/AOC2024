       identification division.
       program-id. AOC1A.

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
           05  LI1            pic 9(5).
           05  FILLER         pic X(3).
           05  LI2            pic 9(5).

       working-storage section.
       01 A-ARB.
          05 INPUT-FS         pic XX.
          05 A-COUNT          pic S9(6) comp-4 value ZERO.

       01 FILLER.
          05 LIST1 occurs 1 to 5000 times depending on A-COUNT 
                              pic 9(5) value HIGH-VALUES.
       01 FILLER.
          05 LIST2 occurs 1 to 5000 times depending on A-COUNT
                              pic 9(5) value HIGH-VALUES.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.

       procedure division.
       A-MAIN section.
           display 'AOC1A' 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF
              add 1 to A-COUNT 
              display LI1 ' www ' LI2  
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           close INFIL
           display A-COUNT ' records read'
           goback
           .
