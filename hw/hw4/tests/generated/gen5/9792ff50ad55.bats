load ../../harness

@test "9792ff50ad55" {
  check 'z     := z   +  -3    ;  skip   ' '⇒ skip; skip, {z → -3}
⇒ skip, {z → -3}'
}
