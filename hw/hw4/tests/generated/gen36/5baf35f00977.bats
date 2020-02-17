load ../../harness

@test "5baf35f00977" {
  check 'Yd    :=    x    -   0   ;
 
x    := -3     - z     ' '⇒ skip; x := (-3-z), {Yd → 0}
⇒ x := (-3-z), {Yd → 0}
⇒ skip, {Yd → 0, x → -3}'
}
