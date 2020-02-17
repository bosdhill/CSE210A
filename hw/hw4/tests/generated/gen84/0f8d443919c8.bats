load ../../harness

@test "0f8d443919c8" {
  check 'if true   then skip      else 

skip     ' '⇒ skip, {}'
}
