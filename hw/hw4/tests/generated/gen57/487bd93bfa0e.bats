load ../../harness

@test "487bd93bfa0e" {
  check 'x  := -4    +x;  


x   :=     x- -3   ' '⇒ skip; x := (x--3), {x → -4}
⇒ x := (x--3), {x → -4}
⇒ skip, {x → -1}'
}
