load ../../harness

@test "8a02f1881b25" {
  check 'x    :=  z +   -3    ;  xe  :=z- z     ' '⇒ skip; xe := (z-z), {x → -3}
⇒ xe := (z-z), {x → -3}
⇒ skip, {x → -3, xe → 0}'
}
