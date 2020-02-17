load ../../harness

@test "79d2fdcb5564" {
  check 'z :=z   +y  ;
skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
