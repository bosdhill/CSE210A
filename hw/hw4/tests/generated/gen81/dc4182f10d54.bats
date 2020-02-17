load ../../harness

@test "dc4182f10d54" {
  check 'while z -    2   =4 +     0  ∧    z     *    -4   <     -3 -   C5     do z   := 4   +z    ' '⇒ skip, {}'
}
