load ../../harness

@test "54b5302e6e61" {
  check 'V   :=     -3    -     x  ;x :=  x  +    -2   ' '⇒ skip; x := (x+-2), {V → -3}
⇒ x := (x+-2), {V → -3}
⇒ skip, {V → -3, x → -2}'
}
