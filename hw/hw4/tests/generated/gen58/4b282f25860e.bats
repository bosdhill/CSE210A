load ../../harness

@test "4b282f25860e" {
  check 'if (4 -    3     <    1 -    b  ∨     true) then      mq := -2 *   x   else y   :=   1  -     0  ' '⇒ mq := (-2*x), {}
⇒ skip, {mq → 0}'
}
