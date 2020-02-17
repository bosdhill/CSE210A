load ../../harness

@test "b9c910a1402a" {
  check 'y:=  -3  *    0;z :=  -4     +    2 ' '⇒ skip; z := (-4+2), {y → 0}
⇒ z := (-4+2), {y → 0}
⇒ skip, {y → 0, z → -2}'
}
