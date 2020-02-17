load ../../harness

@test "afc4aee143bd" {
  check 'x   :=x     *     y     ;
   
x    :=     0+    x   ' '⇒ skip; x := (0+x), {x → 0}
⇒ x := (0+x), {x → 0}
⇒ skip, {x → 0}'
}
