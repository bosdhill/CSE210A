load ../../harness

@test "ffe2f4ba2341" {
  check 'z    :=1    -     0     ; y     :=-2  * f   ' '⇒ skip; y := (-2*f), {z → 1}
⇒ y := (-2*f), {z → 1}
⇒ skip, {y → 0, z → 1}'
}
