load ../../harness

@test "c689dfdfe44c" {
  check 'x    := x    -     y    ;
J   :=  3 ' '⇒ skip; J := 3, {x → 0}
⇒ J := 3, {x → 0}
⇒ skip, {J → 3, x → 0}'
}
