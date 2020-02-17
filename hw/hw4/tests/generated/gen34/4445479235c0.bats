load ../../harness

@test "4445479235c0" {
  check 'while (¬true)     do y:=  x    -   3' '⇒ skip, {}'
}
