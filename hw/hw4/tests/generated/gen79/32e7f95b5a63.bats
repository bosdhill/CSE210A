load ../../harness

@test "32e7f95b5a63" {
  check 'z     :=    2     -  x   ;    
x    :=    -4    ' '⇒ skip; x := -4, {z → 2}
⇒ x := -4, {z → 2}
⇒ skip, {x → -4, z → 2}'
}
