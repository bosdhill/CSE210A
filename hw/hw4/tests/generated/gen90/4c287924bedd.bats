load ../../harness

@test "4c287924bedd" {
  check 'while (¬true)    do skip  ' '⇒ skip, {}'
}
