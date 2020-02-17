load ../../harness

@test "aad7907a68d2" {
  check 'if (-4*    x  =z     *y   ∧  true)     then   
y     :=  -2   *WX    else  
a:=     0    - (0    -1)' '⇒ y := (-2*WX), {}
⇒ skip, {y → 0}'
}
