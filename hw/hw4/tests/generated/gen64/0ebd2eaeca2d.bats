load ../../harness

@test "0ebd2eaeca2d" {
  check 'x   :=   3- z  ;y  :=y*J3    ' '⇒ skip; y := (y*J3), {x → 3}
⇒ y := (y*J3), {x → 3}
⇒ skip, {x → 3, y → 0}'
}
