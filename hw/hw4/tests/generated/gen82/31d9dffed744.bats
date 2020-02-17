load ../../harness

@test "31d9dffed744" {
  check 'x    :=  1   *    4    ;
x    :=z    * z   ' '⇒ skip; x := (z*z), {x → 4}
⇒ x := (z*z), {x → 4}
⇒ skip, {x → 0}'
}
