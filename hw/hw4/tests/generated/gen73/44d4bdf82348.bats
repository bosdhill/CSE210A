load ../../harness

@test "44d4bdf82348" {
  check 'y:=    z   +  x ;

 z:=-3   +  x ' '⇒ skip; z := (-3+x), {y → 0}
⇒ z := (-3+x), {y → 0}
⇒ skip, {y → 0, z → -3}'
}
