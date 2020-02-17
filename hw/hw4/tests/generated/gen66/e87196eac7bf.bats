load ../../harness

@test "e87196eac7bf" {
  check 'z    :=   y -p    ;  x    :=1    -  -1  ' '⇒ skip; x := (1--1), {z → 0}
⇒ x := (1--1), {z → 0}
⇒ skip, {x → 2, z → 0}'
}
