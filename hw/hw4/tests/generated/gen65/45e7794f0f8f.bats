load ../../harness

@test "45e7794f0f8f" {
  check 'if (0     *     -2<   z + 3     ∨   pX-     4  <    0 +    -1)   then y  :=    -3   +    -3 else skip  ' '⇒ y := (-3+-3), {}
⇒ skip, {y → -6}'
}
