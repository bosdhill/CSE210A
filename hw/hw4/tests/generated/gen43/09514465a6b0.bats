load ../../harness

@test "09514465a6b0" {
  check 'if (false   ∨   true)   then y   :=  -4    *    4     else  skip    ' '⇒ y := (-4*4), {}
⇒ skip, {y → -16}'
}
