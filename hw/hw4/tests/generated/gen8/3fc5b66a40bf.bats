load ../../harness

@test "3fc5b66a40bf" {
  check 'x :=x     -    y   ;  
wr   :=    G     -     Q ' '⇒ skip; wr := (G-Q), {x → 0}
⇒ wr := (G-Q), {x → 0}
⇒ skip, {wr → 0, x → 0}'
}
