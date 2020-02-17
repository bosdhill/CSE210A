load ../../harness

@test "189571aef148" {
  check 'while (¬(Q     =     H   +  x)) do gB :=  1   *    -1     ' '⇒ skip, {}'
}
