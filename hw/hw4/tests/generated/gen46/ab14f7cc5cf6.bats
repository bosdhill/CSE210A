load ../../harness

@test "ab14f7cc5cf6" {
  check 'while (¬true)    do y:=  -4  -    3' '⇒ skip, {}'
}
