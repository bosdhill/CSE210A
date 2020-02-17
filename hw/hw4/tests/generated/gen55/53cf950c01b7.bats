load ../../harness

@test "53cf950c01b7" {
  check 'y     :=2     +  y    ;x  :=  y    + z ' '⇒ skip; x := (y+z), {y → 2}
⇒ x := (y+z), {y → 2}
⇒ skip, {x → 2, y → 2}'
}
