load ../../harness

@test "0b0d8cd0d2ff" {
  check 'if (¬(TP +x<    y))  then  z    :=4 * -2      else  x:=     qY   -1 ' '⇒ z := (4*-2), {}
⇒ skip, {z → -8}'
}
