load ../../harness

@test "433979280c74" {
  check 'y  :=   -2  * -3   ' '⇒ skip, {y → 6}'
}
