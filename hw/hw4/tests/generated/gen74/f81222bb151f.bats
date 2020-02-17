load ../../harness

@test "f81222bb151f" {
  check 'skip  ; y  :=  4     +    x' '⇒ y := (4+x), {}
⇒ skip, {y → 4}'
}
