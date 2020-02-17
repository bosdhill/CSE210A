load ../../harness

@test "566dc0138cd7" {
  check 'y  := 2    - -2    ;X    := x+     0    ' '⇒ skip; X := (x+0), {y → 4}
⇒ X := (x+0), {y → 4}
⇒ skip, {X → 0, y → 4}'
}
