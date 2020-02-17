load ../../harness

@test "7482216048b8" {
  check 'z :=0;

skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
