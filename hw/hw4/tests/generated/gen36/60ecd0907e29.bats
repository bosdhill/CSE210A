load ../../harness

@test "60ecd0907e29" {
  check 'if (¬true) then    Z:=K *    z  else  z     :=   C --4     ' '⇒ z := (C--4), {}
⇒ skip, {z → 4}'
}
