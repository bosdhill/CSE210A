load ../../harness

@test "bc3437335b4b" {
  check 'x     := z*-4;   

z   :=   Ks     * z    ' '⇒ skip; z := (Ks*z), {x → 0}
⇒ z := (Ks*z), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
