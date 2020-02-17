load ../../harness

@test "5e509f046ca2" {
  check 'if (1     *     x<     0  *  GW) then   

 x     :=4 *z  else 
x     :=     y    +y   ' '⇒ x := (y+y), {}
⇒ skip, {x → 0}'
}
