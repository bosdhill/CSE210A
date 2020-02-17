load ../../harness

@test "64254c7f9b2a" {
  check 'if (¬(z   -   x     =    y    *     y))    then  


skip      else x:=  -1   - x    ' '⇒ x := (-1-x), {}
⇒ skip, {x → -1}'
}
