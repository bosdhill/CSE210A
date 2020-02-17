load ../../harness

@test "f1824f031890" {
  check 'if (¬false)     then 

  skip    else skip   ' '⇒ skip, {}'
}
