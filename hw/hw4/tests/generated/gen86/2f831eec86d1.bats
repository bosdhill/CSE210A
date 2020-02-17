load ../../harness

@test "2f831eec86d1" {
  check 'if (false   ∨3     *   -1<-1  *     0) then  z    := -4   else  
z  :=  ba*    4   ' '⇒ z := -4, {}
⇒ skip, {z → -4}'
}
