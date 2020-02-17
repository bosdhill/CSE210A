load ../../harness

@test "cfa675709c78" {
  check 'skip  ; x    :=  3     +    x  ' '⇒ x := (3+x), {}
⇒ skip, {x → 3}'
}
