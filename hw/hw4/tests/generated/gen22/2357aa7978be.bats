load ../../harness

@test "2357aa7978be" {
  check 'if (true ∧  true)     then skip else 
skip     ' '⇒ skip, {}'
}
