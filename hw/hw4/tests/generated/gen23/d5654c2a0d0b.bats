load ../../harness

@test "d5654c2a0d0b" {
  check 'y  := -2    -   y ;y     :=    z *     2 ' '⇒ skip; y := (z*2), {y → -2}
⇒ y := (z*2), {y → -2}
⇒ skip, {y → 0}'
}
