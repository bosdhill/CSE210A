load ../../harness

@test "414f73fd5219" {
  check 'y    :=   -1     ;    
y   := y   *     h ' '⇒ skip; y := (y*h), {y → -1}
⇒ y := (y*h), {y → -1}
⇒ skip, {y → 0}'
}
