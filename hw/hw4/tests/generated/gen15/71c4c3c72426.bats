load ../../harness

@test "71c4c3c72426" {
  check 'z :=y    * y  ;skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
