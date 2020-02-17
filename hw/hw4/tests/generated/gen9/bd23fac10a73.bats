load ../../harness

@test "bd23fac10a73" {
  check 'if (1 +-1=    x    -     uD  ∨    -4   <  y     -   -4)  then  
e     :=x  +   2   else 
skip     ' '⇒ e := (x+2), {}
⇒ skip, {e → 2}'
}
