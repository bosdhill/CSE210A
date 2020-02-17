load ../../harness

@test "128de2153a1a" {
  check 'y := y- -1;  y  := -1  -0  ' '⇒ skip; y := (-1-0), {y → 1}
⇒ y := (-1-0), {y → 1}
⇒ skip, {y → -1}'
}
