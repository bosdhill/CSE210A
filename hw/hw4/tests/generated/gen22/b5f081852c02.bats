load ../../harness

@test "b5f081852c02" {
  check 'x     :=     z  -   z ;

 y    :=  x--1 ' '⇒ skip; y := (x--1), {x → 0}
⇒ y := (x--1), {x → 0}
⇒ skip, {x → 0, y → 1}'
}
