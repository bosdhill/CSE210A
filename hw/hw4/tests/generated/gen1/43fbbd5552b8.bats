load ../../harness

@test "43fbbd5552b8" {
  check 'skip  ;  z:= 0    *  -1  ' '⇒ z := (0*-1), {}
⇒ skip, {z → 0}'
}
