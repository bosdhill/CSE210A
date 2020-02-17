load ../../harness

@test "141a0f23891b" {
  check 'if false  then Z :=   1 -     x  else     skip     ' '⇒ skip, {}'
}
