load ../../harness

@test "9e41f76f0afd" {
  check 'z  :=     x     +     z    ;
 
y  :=   -2   *  x' '⇒ skip; y := (-2*x), {z → 0}
⇒ y := (-2*x), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
