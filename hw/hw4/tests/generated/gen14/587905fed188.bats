load ../../harness

@test "587905fed188" {
  check 'skip    ;    
y  := z+ -2' '⇒ y := (z+-2), {}
⇒ skip, {y → -2}'
}
