load ../../harness

@test "93a62a489a20" {
  check 'x:=     z     * -2   ;  x   := -4    -   3   ' '⇒ skip; x := (-4-3), {x → 0}
⇒ x := (-4-3), {x → 0}
⇒ skip, {x → -7}'
}
