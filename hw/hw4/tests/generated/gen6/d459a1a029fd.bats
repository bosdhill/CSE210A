load ../../harness

@test "d459a1a029fd" {
  check 'skip     ;x:= O  +   z    ' '⇒ x := (O+z), {}
⇒ skip, {x → 0}'
}
