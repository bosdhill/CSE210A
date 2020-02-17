load ../../harness

@test "02dcd019e69c" {
  check 'y     :=    x ;skip   ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
