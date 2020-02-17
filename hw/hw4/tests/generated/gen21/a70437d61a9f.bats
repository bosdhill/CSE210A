load ../../harness

@test "a70437d61a9f" {
  check 'if (¬true)    then  y:= 2    *     -3     else  skip ' '⇒ skip, {}'
}
