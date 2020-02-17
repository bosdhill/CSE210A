load ../../harness

@test "4795a1ae594a" {
  check 'while (¬true)  do x := 2   *  y' '⇒ skip, {}'
}
