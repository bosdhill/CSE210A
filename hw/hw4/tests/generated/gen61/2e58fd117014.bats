load ../../harness

@test "2e58fd117014" {
  check 'if (3  -  x =  -2     - 3)     then 
 Zp    :=x  *   x     else  x   :=   x  +  RK   ' '⇒ x := (x+RK), {}
⇒ skip, {x → 0}'
}
