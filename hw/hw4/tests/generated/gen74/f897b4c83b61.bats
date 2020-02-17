load ../../harness

@test "f897b4c83b61" {
  check 'z     :=z    -    4    ;  
 
y     :=  x     +y  ' '⇒ skip; y := (x+y), {z → -4}
⇒ y := (x+y), {z → -4}
⇒ skip, {y → 0, z → -4}'
}
