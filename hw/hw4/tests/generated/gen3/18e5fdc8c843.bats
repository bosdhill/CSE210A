load ../../harness

@test "18e5fdc8c843" {
  check 'if (false∨true)      then  skip     else    skip     ' '⇒ skip, {}'
}
