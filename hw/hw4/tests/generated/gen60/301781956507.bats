load ../../harness

@test "301781956507" {
  check 'y     :=x  * -2  ;   y0   := -1   -    z     ' '⇒ skip; y0 := (-1-z), {y → 0}
⇒ y0 := (-1-z), {y → 0}
⇒ skip, {y → 0, y0 → -1}'
}
