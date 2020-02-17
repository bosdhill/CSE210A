load ../../harness

@test "d7c50ee74abf" {
  check 'z :=   z    *   x  ;    skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
