load ../../harness

@test "2248d3043a8b" {
  check 'y   :=  y   -     0    ;  
W     :=-3     +  -3' '⇒ skip; W := (-3+-3), {y → 0}
⇒ W := (-3+-3), {y → 0}
⇒ skip, {W → -6, y → 0}'
}
