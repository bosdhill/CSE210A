load ../../harness

@test "65255033cb75" {
  check 'y:=x*uM' '⇒ skip, {y → 0}'
}
