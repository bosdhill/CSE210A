load ../../harness

@test "1492f979bb06" {
  check 'z     :=    0  *z   ;     skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
