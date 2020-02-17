load ../../harness

@test "d702e7a098ed" {
  check 'z   :=y   +  -4    ;x     :=   4 - -2  ' '⇒ skip; x := (4--2), {z → -4}
⇒ x := (4--2), {z → -4}
⇒ skip, {x → 6, z → -4}'
}
