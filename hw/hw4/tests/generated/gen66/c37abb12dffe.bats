load ../../harness

@test "c37abb12dffe" {
  check 'if (4     - 3<     3 *y   ∧    false) then  skip     else    
skip    ' '⇒ skip, {}'
}
