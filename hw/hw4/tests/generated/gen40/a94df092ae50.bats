load ../../harness

@test "a94df092ae50" {
  check 'if (0     +z  <    3 -   3   ∧     y   +z< 4     +   1)    then  
 skip   else  
z    :=   x    +   K  ' '⇒ z := (x+K), {}
⇒ skip, {z → 0}'
}
