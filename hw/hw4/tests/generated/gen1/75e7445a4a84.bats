load ../../harness

@test "75e7445a4a84" {
  check 'if (-3 +4    < 1- R4)    then x:= 0    +    i  else  skip ' '⇒ skip, {}'
}
