load ../../harness

@test "b158499967d1" {
  check 'if (¬true)     then  

z     := 2-   0 else  x    :=3     *     y    ' '⇒ x := (3*y), {}
⇒ skip, {x → 0}'
}
