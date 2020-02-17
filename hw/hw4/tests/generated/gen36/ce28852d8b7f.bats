load ../../harness

@test "ce28852d8b7f" {
  check 'p:=    a     +  x;
 


x     := z   *  -2 ' '⇒ skip; x := (z*-2), {p → 0}
⇒ x := (z*-2), {p → 0}
⇒ skip, {p → 0, x → 0}'
}
