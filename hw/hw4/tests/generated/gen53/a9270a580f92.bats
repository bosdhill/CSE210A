load ../../harness

@test "a9270a580f92" {
  check 'if (A  +   -4   =  4 * z ∧   z    +     1     =    -3     +   -4) then  
skip else 
 t :=    z *x  ' '⇒ t := (z*x), {}
⇒ skip, {t → 0}'
}
