load ../../harness

@test "bf4b7e08c985" {
  check 'x     :=z *    1     ; 
skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
