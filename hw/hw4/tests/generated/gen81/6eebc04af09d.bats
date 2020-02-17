load ../../harness

@test "6eebc04af09d" {
  check 'y:=     y + 3   ;y:=    -4   ' '⇒ skip; y := -4, {y → 3}
⇒ y := -4, {y → 3}
⇒ skip, {y → -4}'
}
