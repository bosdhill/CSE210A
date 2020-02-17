load ../../harness

@test "1451d2546bfa" {
  check 'C9     :=x   +    0  ; 

 x     :=     Sn*   3   ' '⇒ skip; x := (Sn*3), {C9 → 0}
⇒ x := (Sn*3), {C9 → 0}
⇒ skip, {C9 → 0, x → 0}'
}
