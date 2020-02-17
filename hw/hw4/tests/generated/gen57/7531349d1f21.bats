load ../../harness

@test "7531349d1f21" {
  check 'z     := -3 ;
skip' '⇒ skip; skip, {z → -3}
⇒ skip, {z → -3}'
}
