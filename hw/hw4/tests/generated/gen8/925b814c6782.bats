load ../../harness

@test "925b814c6782" {
  check 'skip     ;z     :=  s4     -  2 ' '⇒ z := (s4-2), {}
⇒ skip, {z → -2}'
}
