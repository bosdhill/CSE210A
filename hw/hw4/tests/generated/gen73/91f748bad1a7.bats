load ../../harness

@test "91f748bad1a7" {
  check 'while (¬true)      do AW:=x   * 0  ' '⇒ skip, {}'
}
