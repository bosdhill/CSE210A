load ../../harness

@test "b5569873bc2a" {
  check 'p    :=     x    * -2     ;


z:=     z*    x   ' '⇒ skip; z := (z*x), {p → 0}
⇒ z := (z*x), {p → 0}
⇒ skip, {p → 0, z → 0}'
}
