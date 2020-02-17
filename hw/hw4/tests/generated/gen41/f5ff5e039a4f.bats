load ../../harness

@test "f5ff5e039a4f" {
  check 'if (y *  y    <   2 -    z ∧    false)   then  skip    else   skip   ' '⇒ skip, {}'
}
