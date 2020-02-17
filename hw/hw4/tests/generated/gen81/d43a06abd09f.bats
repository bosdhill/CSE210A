load ../../harness

@test "d43a06abd09f" {
  check 'rR    :=   -1*   -2;
x   :=     3    + -3  ' '⇒ skip; x := (3+-3), {rR → 2}
⇒ x := (3+-3), {rR → 2}
⇒ skip, {rR → 2, x → 0}'
}
