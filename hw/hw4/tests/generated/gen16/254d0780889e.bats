load ../../harness

@test "254d0780889e" {
  check 'z     :=  x  * -4     ;U  :=    -4   *0  ' '⇒ skip; U := (-4*0), {z → 0}
⇒ U := (-4*0), {z → 0}
⇒ skip, {U → 0, z → 0}'
}
