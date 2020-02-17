load ../../harness

@test "a8d91ed4fb6a" {
  check 'if (true  ∧ y+   3  < 4     *    y)     then x  :=     3   *    3     else D  :=    x- -1  ' '⇒ D := (x--1), {}
⇒ skip, {D → 1}'
}
