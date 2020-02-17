load ../../harness

@test "4868f6cb8989" {
  check 'z     :=  z     +     -4   ;
y :=  3  ' '⇒ skip; y := 3, {z → -4}
⇒ y := 3, {z → -4}
⇒ skip, {y → 3, z → -4}'
}
