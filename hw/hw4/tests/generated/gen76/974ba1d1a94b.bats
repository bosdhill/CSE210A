load ../../harness

@test "974ba1d1a94b" {
  check 'if (¬false) then 
 x  := -4   *     y    else 

skip    ' '⇒ x := (-4*y), {}
⇒ skip, {x → 0}'
}
