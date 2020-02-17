load ../../harness

@test "0c8493db8271" {
  check 'skip;x:=MX     ' '⇒ x := MX, {}
⇒ skip, {x → 0}'
}
