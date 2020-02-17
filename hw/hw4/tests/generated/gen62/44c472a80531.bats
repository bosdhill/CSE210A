load ../../harness

@test "44c472a80531" {
  check 'y :=  x     +x  ;
x    :=    x +  -3     ' '⇒ skip; x := (x+-3), {y → 0}
⇒ x := (x+-3), {y → 0}
⇒ skip, {x → -3, y → 0}'
}
