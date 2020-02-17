load ../../harness

@test "70d57014b560" {
  check 'LR   :=    y     *  x;  


y   :=   2   + X *     x  ' '⇒ skip; y := (2+(X*x)), {LR → 0}
⇒ y := (2+(X*x)), {LR → 0}
⇒ skip, {LR → 0, y → 2}'
}
