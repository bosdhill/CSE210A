load ../../harness

@test "d52f825b0c31" {
  check 'if (-2  =    s7    -     0∧  true)   then 

 y:= 0     else   y :=     2*K   ' '⇒ y := (2*K), {}
⇒ skip, {y → 0}'
}
