load ../../harness

@test "fa70752b20eb" {
  check 'y:=     2 *y  ;  x:=   z-     -1    ' '⇒ skip; x := (z--1), {y → 0}
⇒ x := (z--1), {y → 0}
⇒ skip, {x → 1, y → 0}'
}
