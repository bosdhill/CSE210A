load ../../harness

@test "d94135f756b0" {
  check 'if true   then 
 


y   := g0   *   y   else skip' '⇒ y := (g0*y), {}
⇒ skip, {y → 0}'
}
