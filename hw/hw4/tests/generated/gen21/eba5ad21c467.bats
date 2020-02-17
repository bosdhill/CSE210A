load ../../harness

@test "eba5ad21c467" {
  check 'XJ := 0    *   x   ; 
x := G   -3   ' '⇒ skip; x := (G-3), {XJ → 0}
⇒ x := (G-3), {XJ → 0}
⇒ skip, {XJ → 0, x → -3}'
}
