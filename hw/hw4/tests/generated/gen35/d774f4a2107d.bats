load ../../harness

@test "d774f4a2107d" {
  check 'while (¬true)     do  skip    ' '⇒ skip, {}'
}
