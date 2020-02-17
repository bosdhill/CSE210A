load ../../harness

@test "5f2d32767001" {
  check 'if (true  ∧     ¬(3  *  y  <     cA -    y))     then 

 x  :=   4-   2   else y    :=1     ' '⇒ x := (4-2), {}
⇒ skip, {x → 2}'
}
