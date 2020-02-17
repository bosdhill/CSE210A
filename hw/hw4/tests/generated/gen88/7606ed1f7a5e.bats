load ../../harness

@test "7606ed1f7a5e" {
  check 'if (¬(-1 <-1    -  z))    then skip   else S4:=   N  +-1 ' '⇒ skip, {}'
}
