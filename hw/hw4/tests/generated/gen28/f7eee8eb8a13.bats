load ../../harness

@test "f7eee8eb8a13" {
  check 'x    := 4   *  z  ; 
y     :=     z*    x   ' '⇒ skip; y := (z*x), {x → 0}
⇒ y := (z*x), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
