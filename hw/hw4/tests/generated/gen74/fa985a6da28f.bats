load ../../harness

@test "fa985a6da28f" {
  check 'y  := B     * 0;
 x    :=     -3  ' '⇒ skip; x := -3, {y → 0}
⇒ x := -3, {y → 0}
⇒ skip, {x → -3, y → 0}'
}
