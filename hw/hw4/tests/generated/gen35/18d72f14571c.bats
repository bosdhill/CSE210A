load ../../harness

@test "18d72f14571c" {
  check 'x     :=z -  z   ;   z :=x *  x  ' '⇒ skip; z := (x*x), {x → 0}
⇒ z := (x*x), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
