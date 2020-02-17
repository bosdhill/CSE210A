load ../../harness

@test "fe54ab1faa9c" {
  check 'if (false  ∧    1 *  y    <    4  +  2)    then    
y     :=     x    -    3      else x :=    -2    + y   ' '⇒ x := (-2+y), {}
⇒ skip, {x → -2}'
}
