load ../../harness

@test "09df8bc861de" {
  check 'y :=    gL   +  x     ;
z:=     -2  +4 ' '⇒ skip; z := (-2+4), {y → 0}
⇒ z := (-2+4), {y → 0}
⇒ skip, {y → 0, z → 2}'
}
