load ../../harness

@test "6275d98f7e4c" {
  check 'if (rM *    2<     2  *    z    ∧   false)    then skip  else skip     ' '⇒ skip, {}'
}
