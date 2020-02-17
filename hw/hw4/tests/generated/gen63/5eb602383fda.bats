load ../../harness

@test "5eb602383fda" {
  check 'while false   ∨     4    * x    =SJ +-4 do x:=y    -   Y ' '⇒ skip, {}'
}
