load ../../harness

@test "fd646e63c9f8" {
  check 'z:=  z    ; 
y     :=  Bu -  x  ' '⇒ skip; y := (Bu-x), {z → 0}
⇒ y := (Bu-x), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
