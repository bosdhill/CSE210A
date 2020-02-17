load ../../harness

@test "e1250494842b" {
  check 'x:=   JJ *   b4    ;z   :=     y-    z  ' '⇒ skip; z := (y-z), {x → 0}
⇒ z := (y-z), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
