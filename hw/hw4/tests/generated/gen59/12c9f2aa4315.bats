load ../../harness

@test "12c9f2aa4315" {
  check 'z   :=   y    *-1  ;
 D  :=   -1   -  -3   ' '⇒ skip; D := (-1--3), {z → 0}
⇒ D := (-1--3), {z → 0}
⇒ skip, {D → 2, z → 0}'
}
