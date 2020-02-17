load ../../harness

@test "ce68391b1f54" {
  check 'if (¬(4    -     4    <     4 + -2))      then 
x   :=    -3   -     y else  
x:=-1    + -3     ' '⇒ x := (-1+-3), {}
⇒ skip, {x → -4}'
}
