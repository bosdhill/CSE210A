load ../../harness

@test "8edad503438b" {
  check 'if (¬true)     then y:= y  -    x else skip  ' '⇒ skip, {}'
}
