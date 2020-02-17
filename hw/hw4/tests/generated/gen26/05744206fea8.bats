load ../../harness

@test "05744206fea8" {
  check 'C     :=-3 +    z;  x  := 2  -    x    ' '⇒ skip; x := (2-x), {C → -3}
⇒ x := (2-x), {C → -3}
⇒ skip, {C → -3, x → 2}'
}
