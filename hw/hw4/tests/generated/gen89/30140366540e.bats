load ../../harness

@test "30140366540e" {
  check 'y  :=  z   +   J5    ; z :=-2     -    (y   +     4) ' '⇒ skip; z := (-2-(y+4)), {y → 0}
⇒ z := (-2-(y+4)), {y → 0}
⇒ skip, {y → 0, z → -6}'
}
