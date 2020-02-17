load ../../harness

@test "41e9db227270" {
  check 'z   :=     2  -    y  ;ne :=     -2 -  3 ' '⇒ skip; ne := (-2-3), {z → 2}
⇒ ne := (-2-3), {z → 2}
⇒ skip, {ne → -5, z → 2}'
}
