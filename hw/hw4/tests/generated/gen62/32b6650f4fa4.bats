load ../../harness

@test "32b6650f4fa4" {
  check 'y    :=  z;
skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
