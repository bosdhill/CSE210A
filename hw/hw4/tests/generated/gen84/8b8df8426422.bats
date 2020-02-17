load ../../harness

@test "8b8df8426422" {
  check 'if (true∨ -3 +     z<  4   +    y) then x :=    -4     +   JT else 
 skip' '⇒ x := (-4+JT), {}
⇒ skip, {x → -4}'
}
