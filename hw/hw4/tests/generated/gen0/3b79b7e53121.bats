load ../../harness

@test "3b79b7e53121" {
  check 'y :=-3   -   z     ; skip' '⇒ skip; skip, {y → -3}
⇒ skip, {y → -3}'
}
