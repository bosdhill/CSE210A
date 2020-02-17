load ../../harness

@test "cfe6e795c9c8" {
  check 'z     :=  x  *x    ;
 
 
i  :=    Yo *   z    ' '⇒ skip; i := (Yo*z), {z → 0}
⇒ i := (Yo*z), {z → 0}
⇒ skip, {i → 0, z → 0}'
}
