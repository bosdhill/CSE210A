load ../../harness

@test "0c3c720c5a03" {
  check 'y:=z-  (y  --2)  ' '⇒ skip, {y → -2}'
}
