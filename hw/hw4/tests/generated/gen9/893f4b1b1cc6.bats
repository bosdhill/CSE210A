load ../../harness

@test "893f4b1b1cc6" {
  check 'z    :=   1   + 0  ; 

z   :=   -4  *  z    ' '⇒ skip; z := (-4*z), {z → 1}
⇒ z := (-4*z), {z → 1}
⇒ skip, {z → -4}'
}
