load ../../harness

@test "10d9808c53bb" {
  check 'y     :=    y     *2;

skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
