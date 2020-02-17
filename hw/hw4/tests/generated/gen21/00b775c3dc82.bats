load ../../harness

@test "00b775c3dc82" {
  check 'while (¬true)    do    T  :=   4  -  z  ' '⇒ skip, {}'
}
