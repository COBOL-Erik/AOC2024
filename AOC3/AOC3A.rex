/* Rexx (TSO) */
NUMERIC DIGITS 20
drop input.
dsn='xxx'
"ALLOC DA('"dsn"') FI(INFIL2) SHR REUSE"
"EXECIO * DISKR INFIL2 (STEM input. FINIS"
"FREE FI(INFIL2)"
acc = 0
do i = 1 to input.0
   text = space(input.i)
   do until text = ''
      parse var text . 'mul(' rest1
      parse var rest1 n1 ',' n2 ')' rest2
      if datatype(n1) = 'NUM' & datatype(n2) = 'NUM' then do
         acc = acc + (n1 * n2)
         text = rest2
      end
      else
         text = rest1
   end
end
say acc
exit 0
