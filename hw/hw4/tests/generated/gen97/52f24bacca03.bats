load ../../harness

@test "52f24bacca03" {
  check 'while (¬true)    do  skip    ' '⇒ skip, {}'
}
