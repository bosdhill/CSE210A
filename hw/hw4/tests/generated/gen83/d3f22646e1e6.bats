load ../../harness

@test "d3f22646e1e6" {
  check 'y:=  x     -    4   ;
x     :=  2 - z    ' '⇒ skip; x := (2-z), {y → -4}
⇒ x := (2-z), {y → -4}
⇒ skip, {x → 2, y → -4}'
}
