load ../../harness

@test "27ae48877aa5" {
  check 'cF  :=     4     +   z     ;  x     :=     y     +    x  ' '⇒ skip; x := (y+x), {cF → 4}
⇒ x := (y+x), {cF → 4}
⇒ skip, {cF → 4, x → 0}'
}
