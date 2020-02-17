load ../../harness

@test "c68a51449052" {
  check 'y:=    x   - y ' '⇒ skip, {y → 0}'
}
