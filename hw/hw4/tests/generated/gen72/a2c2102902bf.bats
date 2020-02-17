load ../../harness

@test "a2c2102902bf" {
  check 'zh :=  y +4   ; y  :=0-   2    ' '⇒ skip; y := (0-2), {zh → 4}
⇒ y := (0-2), {zh → 4}
⇒ skip, {y → -2, zh → 4}'
}
