load ../../harness

@test "69415f41317e" {
  check 'z:=2     +4 ; 

  y  :=  z     + -2    ' '⇒ skip; y := (z+-2), {z → 6}
⇒ y := (z+-2), {z → 6}
⇒ skip, {y → 4, z → 6}'
}
