load ../../harness

@test "10a81c2cf6ef" {
  check 'if (¬(x     +x=    -3    +    z)) then 
 x   :=y  - y     else   
Tl  :=    4   +    z    ' '⇒ x := (y-y), {}
⇒ skip, {x → 0}'
}
