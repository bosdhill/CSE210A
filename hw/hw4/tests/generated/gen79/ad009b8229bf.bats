load ../../harness

@test "ad009b8229bf" {
  check 'x :=    2 +    4 ;
 
 z   := -4  *B ' '⇒ skip; z := (-4*B), {x → 6}
⇒ z := (-4*B), {x → 6}
⇒ skip, {x → 6, z → 0}'
}
