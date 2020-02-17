load ../../harness

@test "48eaa7170642" {
  check 'while false ∧   2   +   x =    z   - x     do x:=    4     +    y    ' '⇒ skip, {}'
}
