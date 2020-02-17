load ../../harness

@test "9367213198c1" {
  check 'while false∨0     + 0   < 3*     z    do   skip     ' '⇒ skip, {}'
}
