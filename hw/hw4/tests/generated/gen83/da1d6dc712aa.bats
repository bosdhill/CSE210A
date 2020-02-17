load ../../harness

@test "da1d6dc712aa" {
  check 'y   := 0  *     -3  ;x     :=  3  *    -3 ; 


z :=4     +   z     ' '⇒ skip; x := (3*-3); z := (4+z), {y → 0}
⇒ x := (3*-3); z := (4+z), {y → 0}
⇒ skip; z := (4+z), {x → -9, y → 0}
⇒ z := (4+z), {x → -9, y → 0}
⇒ skip, {x → -9, y → 0, z → 4}'
}
