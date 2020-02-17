load ../../harness

@test "3f2cfb66fd71" {
  check 'x    :=y     +   y   ;
 skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
