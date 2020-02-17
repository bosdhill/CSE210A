load ../../harness

@test "35285a92009f" {
  check 'y  :=   x-     z; x :=   3  *    x   ' '⇒ skip; x := (3*x), {y → 0}
⇒ x := (3*x), {y → 0}
⇒ skip, {x → 0, y → 0}'
}
