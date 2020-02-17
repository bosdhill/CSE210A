load ../../harness

@test "b5a9345a3f33" {
  check 'skip    ; x   :=3  +    y   ' '⇒ x := (3+y), {}
⇒ skip, {x → 3}'
}
