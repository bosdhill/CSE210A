load ../../harness

@test "15e5f3f1cc12" {
  check 'ck  :=     1 ;    x := x     * 4     ' '⇒ skip; x := (x*4), {ck → 1}
⇒ x := (x*4), {ck → 1}
⇒ skip, {ck → 1, x → 0}'
}
