load ../../harness

@test "184ffd6ec7d1" {
  check 'while (¬true)      do x:=  y    -z   ' '⇒ skip, {}'
}
