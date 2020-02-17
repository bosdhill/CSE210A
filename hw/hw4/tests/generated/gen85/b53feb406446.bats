load ../../harness

@test "b53feb406446" {
  check 'y     :=   -1;
  skip' '⇒ skip; skip, {y → -1}
⇒ skip, {y → -1}'
}
