load ../../harness

@test "c8afd0ac4867" {
  check 'if (y    *   4   = z *-2     ∨  -4     <p) then v9  := x     + -3   else   skip' '⇒ v9 := (x+-3), {}
⇒ skip, {v9 → -3}'
}
