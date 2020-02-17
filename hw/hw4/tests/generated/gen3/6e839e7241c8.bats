load ../../harness

@test "6e839e7241c8" {
  check 'if (jz   -  4    <   3  +z)    then skip     else z   :=    3 +  -3' '⇒ skip, {}'
}
