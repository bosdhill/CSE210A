load ../../harness

@test "8e1b3ed171fa" {
  check 'x:=  4    *     -2 ; 

y :=  3-  y' '⇒ skip; y := (3-y), {x → -8}
⇒ y := (3-y), {x → -8}
⇒ skip, {x → -8, y → 3}'
}
