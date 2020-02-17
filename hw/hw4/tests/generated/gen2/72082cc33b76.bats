load ../../harness

@test "72082cc33b76" {
  check 'z     := c ;  
 y  :=    z     + 3   ' '⇒ skip; y := (z+3), {z → 0}
⇒ y := (z+3), {z → 0}
⇒ skip, {y → 3, z → 0}'
}
