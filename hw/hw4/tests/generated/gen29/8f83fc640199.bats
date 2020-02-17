load ../../harness

@test "8f83fc640199" {
  check 'x:=    y   -z' '⇒ skip, {x → 0}'
}
