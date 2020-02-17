load ../../harness

@test "cf3c3dade59e" {
  check 'if (false∧ true)     then  skip   else   y    :=-1-   z     ' '⇒ y := (-1-z), {}
⇒ skip, {y → -1}'
}
