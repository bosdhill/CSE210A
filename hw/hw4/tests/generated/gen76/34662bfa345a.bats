load ../../harness

@test "34662bfa345a" {
  check 'skip   ;x :=  y-   x  ' '⇒ x := (y-x), {}
⇒ skip, {x → 0}'
}
