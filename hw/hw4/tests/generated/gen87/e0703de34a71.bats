load ../../harness

@test "e0703de34a71" {
  check 'z:=  4-  y;  
y     :=   z     - h     ' '⇒ skip; y := (z-h), {z → 4}
⇒ y := (z-h), {z → 4}
⇒ skip, {y → 4, z → 4}'
}
