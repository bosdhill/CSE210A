load ../../harness

@test "92bf9e8a23e0" {
  check 'if (1     *z   = z  +  qm)   then  
x :=2  *     y else skip     ' '⇒ x := (2*y), {}
⇒ skip, {x → 0}'
}
