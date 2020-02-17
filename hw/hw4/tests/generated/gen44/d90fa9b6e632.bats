load ../../harness

@test "d90fa9b6e632" {
  check 'if (H   -    3     <z  +    z ∨  true)      then  
z    :=  1    +  xE     else  
   skip' '⇒ z := (1+xE), {}
⇒ skip, {z → 1}'
}
