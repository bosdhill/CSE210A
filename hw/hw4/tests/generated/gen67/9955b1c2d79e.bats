load ../../harness

@test "9955b1c2d79e" {
  check 'if (true ∨    y    *  z  =    -4   -    Ao)      then    
z    := AK      else 
   z := 4-  3   ' '⇒ z := AK, {}
⇒ skip, {z → 0}'
}
