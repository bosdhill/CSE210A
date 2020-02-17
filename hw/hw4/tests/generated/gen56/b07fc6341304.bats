load ../../harness

@test "b07fc6341304" {
  check 'while (¬(-4    -     -2 <   0 -  x)) do y  :=  z *   x ' '⇒ skip, {}'
}
