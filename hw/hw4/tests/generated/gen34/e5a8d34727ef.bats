load ../../harness

@test "e5a8d34727ef" {
  check 'if (¬(x   =    y     +     4))    then skip  else   skip ' '⇒ skip, {}'
}
