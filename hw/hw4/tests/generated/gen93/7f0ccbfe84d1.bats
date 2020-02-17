load ../../harness

@test "7f0ccbfe84d1" {
  check 'x  :=  x     ; 
y    :=  x     ' '⇒ skip; y := x, {x → 0}
⇒ y := x, {x → 0}
⇒ skip, {x → 0, y → 0}'
}
