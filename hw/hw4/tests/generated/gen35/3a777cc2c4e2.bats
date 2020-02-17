load ../../harness

@test "3a777cc2c4e2" {
  check 'x:=    3 -     0  ;
skip     ;
y    :=  x- -1   ' '⇒ skip; skip; y := (x--1), {x → 3}
⇒ skip; y := (x--1), {x → 3}
⇒ y := (x--1), {x → 3}
⇒ skip, {x → 3, y → 4}'
}
