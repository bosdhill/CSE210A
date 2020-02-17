load ../../harness

@test "ba5520e92b94" {
  check 'x:=     -4 +-3  ' '⇒ skip, {x → -7}'
}
