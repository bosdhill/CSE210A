load ../../harness

@test "460a01c31d91" {
  check 'x   :=  R0 +     -3 ;
 
y    :=   3  + 1 ' '⇒ skip; y := (3+1), {x → -3}
⇒ y := (3+1), {x → -3}
⇒ skip, {x → -3, y → 4}'
}
