load ../../harness

@test "cae4f0470ded" {
  check 'x     :=  -4     +   1   ;
   z :=     4- 3   ' '⇒ skip; z := (4-3), {x → -3}
⇒ z := (4-3), {x → -3}
⇒ skip, {x → -3, z → 1}'
}
