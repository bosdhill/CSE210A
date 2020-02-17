load ../../harness

@test "5bc7ff6ed084" {
  check 'z:=    z    *     -1   ;O :=  -3     +   -2    ' '⇒ skip; O := (-3+-2), {z → 0}
⇒ O := (-3+-2), {z → 0}
⇒ skip, {O → -5, z → 0}'
}
