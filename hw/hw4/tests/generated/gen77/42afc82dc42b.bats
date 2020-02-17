load ../../harness

@test "42afc82dc42b" {
  check 'x     :=4   +     y   ; 
skip   ' '⇒ skip; skip, {x → 4}
⇒ skip, {x → 4}'
}
