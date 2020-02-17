load ../../harness

@test "0c933dbd4410" {
  check 'x:= z     -     -3  ; x     :=     3 ' '⇒ skip; x := 3, {x → 3}
⇒ x := 3, {x → 3}
⇒ skip, {x → 3}'
}
