load ../../harness

@test "fbc2f139633e" {
  check 'x :=   z-    -4  ;y :=  z     --1     ' '⇒ skip; y := (z--1), {x → 4}
⇒ y := (z--1), {x → 4}
⇒ skip, {x → 4, y → 1}'
}
