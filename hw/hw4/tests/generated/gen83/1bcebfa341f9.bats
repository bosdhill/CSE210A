load ../../harness

@test "1bcebfa341f9" {
  check 'x :=     b *    3    ;
 x    := z    - -2   ' '⇒ skip; x := (z--2), {x → 0}
⇒ x := (z--2), {x → 0}
⇒ skip, {x → 2}'
}
