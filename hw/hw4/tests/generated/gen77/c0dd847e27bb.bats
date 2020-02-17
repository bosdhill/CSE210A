load ../../harness

@test "c0dd847e27bb" {
  check 'A    :=r    +   -1   ;
x := y*    x   ' '⇒ skip; x := (y*x), {A → -1}
⇒ x := (y*x), {A → -1}
⇒ skip, {A → -1, x → 0}'
}
