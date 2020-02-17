load ../../harness

@test "f99bd69260ad" {
  check 'x   :=     -4- Tm ;
 
  z  := p1   + x' '⇒ skip; z := (p1+x), {x → -4}
⇒ z := (p1+x), {x → -4}
⇒ skip, {x → -4, z → -4}'
}
