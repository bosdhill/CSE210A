load ../../harness

@test "18c64e6b284e" {
  check 'x    :=    z +     -4   ;  
 
z  :=  z -x ' '⇒ skip; z := (z-x), {x → -4}
⇒ z := (z-x), {x → -4}
⇒ skip, {x → -4, z → 4}'
}
