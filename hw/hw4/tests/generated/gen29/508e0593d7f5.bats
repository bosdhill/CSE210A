load ../../harness

@test "508e0593d7f5" {
  check 'Z5  :=  z - 3  ;   x :=x   +   0  ' '⇒ skip; x := (x+0), {Z5 → -3}
⇒ x := (x+0), {Z5 → -3}
⇒ skip, {Z5 → -3, x → 0}'
}
