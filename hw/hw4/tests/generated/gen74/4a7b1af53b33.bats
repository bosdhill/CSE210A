load ../../harness

@test "4a7b1af53b33" {
  check 'if (y + y < 1 +    x    ∨   true) then  
y    :=  z    else  
 skip    ' '⇒ y := z, {}
⇒ skip, {y → 0}'
}
