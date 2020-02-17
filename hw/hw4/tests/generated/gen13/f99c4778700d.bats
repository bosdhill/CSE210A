load ../../harness

@test "f99c4778700d" {
  check 'z:=   -1    -  z ' '⇒ skip, {z → -1}'
}
