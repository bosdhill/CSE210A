load ../../harness

@test "66c6db33d23d" {
  check 'x    :=0 + WU     ;a6    :=  -3    *    z  ' '⇒ skip; a6 := (-3*z), {x → 0}
⇒ a6 := (-3*z), {x → 0}
⇒ skip, {a6 → 0, x → 0}'
}
