load ../../harness

@test "6dbc4ba71ff1" {
  check 'hj :=     2  +-3    ;
x     :=0  -   1     ' '⇒ skip; x := (0-1), {hj → -1}
⇒ x := (0-1), {hj → -1}
⇒ skip, {hj → -1, x → -1}'
}
