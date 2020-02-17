load ../../harness

@test "71d8adf6f426" {
  check 'x  :=     T  *   -2   ; 

 x:=    2 -   3     ' '⇒ skip; x := (2-3), {x → 0}
⇒ x := (2-3), {x → 0}
⇒ skip, {x → -1}'
}
