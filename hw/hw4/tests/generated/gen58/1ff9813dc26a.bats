load ../../harness

@test "1ff9813dc26a" {
  check 'x    :=  y   + z  ;  
 skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
