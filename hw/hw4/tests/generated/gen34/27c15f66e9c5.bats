load ../../harness

@test "27c15f66e9c5" {
  check 'x     :=     -3  -    -2 ; Pk     := -3  +     z   ' '⇒ skip; Pk := (-3+z), {x → -1}
⇒ Pk := (-3+z), {x → -1}
⇒ skip, {Pk → -3, x → -1}'
}
