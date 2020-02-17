load ../../harness

@test "695dba3cb803" {
  check 'x   :=     x;
 uB  :=    3 *   y    ' '⇒ skip; uB := (3*y), {x → 0}
⇒ uB := (3*y), {x → 0}
⇒ skip, {uB → 0, x → 0}'
}
