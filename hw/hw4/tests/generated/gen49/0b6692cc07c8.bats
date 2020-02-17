load ../../harness

@test "0b6692cc07c8" {
  check 'if (true∧    false)     then    

skip    else 

 x    := -4  +     z  ' '⇒ x := (-4+z), {}
⇒ skip, {x → -4}'
}
