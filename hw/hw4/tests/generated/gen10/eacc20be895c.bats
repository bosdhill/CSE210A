load ../../harness

@test "eacc20be895c" {
  check 'if (¬true)      then  
x :=dz  *    3  else x   := v   +     -4     ' '⇒ x := (v+-4), {}
⇒ skip, {x → -4}'
}
