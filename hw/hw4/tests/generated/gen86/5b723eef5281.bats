load ../../harness

@test "5b723eef5281" {
  check 'z:=-2 -  0     ;
y    := 1* x     ' '⇒ skip; y := (1*x), {z → -2}
⇒ y := (1*x), {z → -2}
⇒ skip, {y → 0, z → -2}'
}
