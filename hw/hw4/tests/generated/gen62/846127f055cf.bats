load ../../harness

@test "846127f055cf" {
  check 'z   :=     1     -    y ;
 z     :=   2   *   -1' '⇒ skip; z := (2*-1), {z → 1}
⇒ z := (2*-1), {z → 1}
⇒ skip, {z → -2}'
}
