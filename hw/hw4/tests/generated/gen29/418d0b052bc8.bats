load ../../harness

@test "418d0b052bc8" {
  check 'x    :=     -1     + x     ;  y4  :=   z   +   z     ' '⇒ skip; y4 := (z+z), {x → -1}
⇒ y4 := (z+z), {x → -1}
⇒ skip, {x → -1, y4 → 0}'
}
