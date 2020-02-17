load ../../harness

@test "d940b4e753bc" {
  check 'y :=   e8*     0   ; x :=  hy     -  y    ' '⇒ skip; x := (hy-y), {y → 0}
⇒ x := (hy-y), {y → 0}
⇒ skip, {x → 0, y → 0}'
}
