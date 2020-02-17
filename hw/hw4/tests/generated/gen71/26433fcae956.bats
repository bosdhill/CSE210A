load ../../harness

@test "26433fcae956" {
  check 'y  :=  z   - 4   ;skip' '⇒ skip; skip, {y → -4}
⇒ skip, {y → -4}'
}
