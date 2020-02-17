load ../../harness

@test "9010c8bcbf89" {
  check 'z :=   -3-z     ;  y :=-2  + 1  ' '⇒ skip; y := (-2+1), {z → -3}
⇒ y := (-2+1), {z → -3}
⇒ skip, {y → -1, z → -3}'
}
