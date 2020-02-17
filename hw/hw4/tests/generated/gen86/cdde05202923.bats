load ../../harness

@test "cdde05202923" {
  check 'if (¬(2    *   z  <     -2   +    -1))    then 
 rp     :=     1+ x   else  pZ     :=     x   -  -3' '⇒ rp := (1+x), {}
⇒ skip, {rp → 1}'
}
