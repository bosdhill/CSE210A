load ../../harness

@test "c348606fb6d4" {
  check 'z    :=x  ;
skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
