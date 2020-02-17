load ../../harness

@test "1caa62d8cc3e" {
  check 'if (y   *y    =  y  +  -4 ∧  -4     -   x  =    0     +   z)      then 



skip    else 
z :=    x    -   y   ' '⇒ z := (x-y), {}
⇒ skip, {z → 0}'
}
