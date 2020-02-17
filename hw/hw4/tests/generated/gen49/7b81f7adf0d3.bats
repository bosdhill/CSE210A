load ../../harness

@test "7b81f7adf0d3" {
  check 'o5  :=   -3  +     4   ;
   k:=  -2     *   -2  ' '⇒ skip; k := (-2*-2), {o5 → 1}
⇒ k := (-2*-2), {o5 → 1}
⇒ skip, {k → 4, o5 → 1}'
}
