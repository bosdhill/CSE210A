load ../../harness

@test "ae959d999154" {
  check 'z := 3  *   x   ;FP    :=  x     *    z' '⇒ skip; FP := (x*z), {z → 0}
⇒ FP := (x*z), {z → 0}
⇒ skip, {FP → 0, z → 0}'
}
