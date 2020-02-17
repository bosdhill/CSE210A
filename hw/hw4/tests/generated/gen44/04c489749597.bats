load ../../harness

@test "04c489749597" {
  check 'if (2  -     x    =    4    +  y     ∧   true)      then 
   x   :=    Q    -   0    else 
y     :=  3     *  0  ' '⇒ y := (3*0), {}
⇒ skip, {y → 0}'
}
