load ../../harness

@test "c2072b71142a" {
  check 'if (nc     +    y <    z   +     2   ∧ z  -    z =    y   *-3)      then 


x:=-4    + z     else  

x :=    0   +    0    ' '⇒ x := (-4+z), {}
⇒ skip, {x → -4}'
}
