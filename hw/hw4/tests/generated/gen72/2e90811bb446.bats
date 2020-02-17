load ../../harness

@test "2e90811bb446" {
  check 'y  := -3 *   0     ;  
x   :=  -4  ' '⇒ skip; x := -4, {y → 0}
⇒ x := -4, {y → 0}
⇒ skip, {x → -4, y → 0}'
}
