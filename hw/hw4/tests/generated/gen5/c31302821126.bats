load ../../harness

@test "c31302821126" {
  check 'U     :=   z     *f   ; 
g    := y*     1  ' '⇒ skip; g := (y*1), {U → 0}
⇒ g := (y*1), {U → 0}
⇒ skip, {U → 0, g → 0}'
}
