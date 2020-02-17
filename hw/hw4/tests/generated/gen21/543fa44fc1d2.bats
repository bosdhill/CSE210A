load ../../harness

@test "543fa44fc1d2" {
  check 'z :=  K     +   z ;skip   ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
