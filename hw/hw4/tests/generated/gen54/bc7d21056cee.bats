load ../../harness

@test "bc7d21056cee" {
  check 'z :=   1  +   -4 ' '⇒ skip, {z → -3}'
}
