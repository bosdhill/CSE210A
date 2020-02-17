load ../../harness

@test "921c0d801e9d" {
  check 'if false     then 
 F4    :=     -3  *   3   else 
x   :=   x    +    -4 ' '⇒ x := (x+-4), {}
⇒ skip, {x → -4}'
}
