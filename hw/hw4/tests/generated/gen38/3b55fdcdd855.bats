load ../../harness

@test "3b55fdcdd855" {
  check 'z :=     1    *   x    ;   skip   ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
