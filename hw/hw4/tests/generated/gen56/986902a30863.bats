load ../../harness

@test "986902a30863" {
  check 'DY:=    x;

x   :=  -1    + -3    ' '⇒ skip; x := (-1+-3), {DY → 0}
⇒ x := (-1+-3), {DY → 0}
⇒ skip, {DY → 0, x → -4}'
}
