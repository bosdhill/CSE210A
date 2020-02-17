load ../../harness

@test "a13500f95b1f" {
  check 'z  :=-1    ;
 bw     :=   x     ' '⇒ skip; bw := x, {z → -1}
⇒ bw := x, {z → -1}
⇒ skip, {bw → 0, z → -1}'
}
