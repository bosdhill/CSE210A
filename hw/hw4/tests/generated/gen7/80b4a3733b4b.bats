load ../../harness

@test "80b4a3733b4b" {
  check 'if true    then 
   x  :=    -1  -    x     else 
   x    :=  A     *at    ' '⇒ x := (-1-x), {}
⇒ skip, {x → -1}'
}
