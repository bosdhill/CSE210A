load ../../harness

@test "a86fe082f641" {
  check 'if (-2*   S1     =   4+     x  ∨    false) then 
  skip     else   
z:=    Rh    *  3' '⇒ z := (Rh*3), {}
⇒ skip, {z → 0}'
}
