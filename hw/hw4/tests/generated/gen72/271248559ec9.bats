load ../../harness

@test "271248559ec9" {
  check 'x   :=2 +    y   ;   skip   ' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
