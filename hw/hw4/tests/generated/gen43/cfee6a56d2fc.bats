load ../../harness

@test "cfee6a56d2fc" {
  check 'z    :=3   +  -4 ;   y   :=-4    *   3  ' '⇒ skip; y := (-4*3), {z → -1}
⇒ y := (-4*3), {z → -1}
⇒ skip, {y → -12, z → -1}'
}
