load ../../harness

@test "4ca31a585dcd" {
  check 'z   :=  r     +     -4     ; 
y  :=  y+ -3 ' '⇒ skip; y := (y+-3), {z → -4}
⇒ y := (y+-3), {z → -4}
⇒ skip, {y → -3, z → -4}'
}
