load ../../harness

@test "4cf16b190790" {
  check 'while false∧   y    *   y =    2     -z     do skip     ' '⇒ skip, {}'
}
