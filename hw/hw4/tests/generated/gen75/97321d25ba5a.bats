load ../../harness

@test "97321d25ba5a" {
  check 'while y <  x do   x :=    y ' '⇒ skip, {}'
}
