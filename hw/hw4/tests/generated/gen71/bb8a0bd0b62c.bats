load ../../harness

@test "bb8a0bd0b62c" {
  check 'y := 3-  x   ;skip    ' '⇒ skip; skip, {y → 3}
⇒ skip, {y → 3}'
}
