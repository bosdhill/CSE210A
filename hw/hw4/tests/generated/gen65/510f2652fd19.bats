load ../../harness

@test "510f2652fd19" {
  check 'if (true  ∨    x -     0     =g    -    y) then z     :=     dx *     4 else skip ' '⇒ z := (dx*4), {}
⇒ skip, {z → 0}'
}
