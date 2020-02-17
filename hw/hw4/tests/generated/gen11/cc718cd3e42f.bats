load ../../harness

@test "cc718cd3e42f" {
  check 'if (¬false)     then skip  else 
  skip  ' '⇒ skip, {}'
}
