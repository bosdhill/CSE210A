load ../../harness

@test "35df773c856a" {
  check 'y    :=   z *x     ;
x  :=    z   -    -3 ' '⇒ skip; x := (z--3), {y → 0}
⇒ x := (z--3), {y → 0}
⇒ skip, {x → 3, y → 0}'
}
