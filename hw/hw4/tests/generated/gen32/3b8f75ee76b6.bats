load ../../harness

@test "3b8f75ee76b6" {
  check 'if (x   =z   +     -3 ∧  D  +   0     < z     -    -3)   then 
z:=     y  *  z     else R  :=   2   +   z  ' '⇒ R := (2+z), {}
⇒ skip, {R → 2}'
}
