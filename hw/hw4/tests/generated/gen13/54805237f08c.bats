load ../../harness

@test "54805237f08c" {
  check 'x:=y   *   z' '⇒ skip, {x → 0}'
}
