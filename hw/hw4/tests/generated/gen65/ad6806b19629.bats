load ../../harness

@test "ad6806b19629" {
  check 'G6:= c     ;y:=     -4     *    k  ' '⇒ skip; y := (-4*k), {G6 → 0}
⇒ y := (-4*k), {G6 → 0}
⇒ skip, {G6 → 0, y → 0}'
}
