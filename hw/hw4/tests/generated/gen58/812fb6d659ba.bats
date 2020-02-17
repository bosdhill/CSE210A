load ../../harness

@test "812fb6d659ba" {
  check 'y:=  y   *    y     ' '⇒ skip, {y → 0}'
}
