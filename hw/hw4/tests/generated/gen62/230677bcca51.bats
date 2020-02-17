load ../../harness

@test "230677bcca51" {
  check 'if (true  ∨true)   then    y   :=     Z   +   x    else 
  
x   :=  z    * -3' '⇒ y := (Z+x), {}
⇒ skip, {y → 0}'
}
