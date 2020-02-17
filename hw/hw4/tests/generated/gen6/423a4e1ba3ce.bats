load ../../harness

@test "423a4e1ba3ce" {
  check 'while (¬true)  do     skip  ' '⇒ skip, {}'
}
