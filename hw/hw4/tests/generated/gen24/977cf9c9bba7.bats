load ../../harness

@test "977cf9c9bba7" {
  check 'if (¬(x+y <  y  - 2)) then     x   := x else   {x    :=x   *y  ;skip}    ' '⇒ x := x, {}
⇒ skip, {x → 0}'
}
