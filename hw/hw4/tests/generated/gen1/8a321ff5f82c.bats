load ../../harness

@test "8a321ff5f82c" {
  check 'if (¬true)  then  skip     else  x    :=     y   -z     ' '⇒ x := (y-z), {}
⇒ skip, {x → 0}'
}
