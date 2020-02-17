load ../../harness

@test "dc04d29f0108" {
  check 'skip    ;j:=x ' '⇒ j := x, {}
⇒ skip, {j → 0}'
}
