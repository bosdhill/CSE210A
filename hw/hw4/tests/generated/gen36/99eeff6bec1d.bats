load ../../harness

@test "99eeff6bec1d" {
  check 'a   :=   x     +     x  ;
  y :=    z+   -3 ' '⇒ skip; y := (z+-3), {a → 0}
⇒ y := (z+-3), {a → 0}
⇒ skip, {a → 0, y → -3}'
}
