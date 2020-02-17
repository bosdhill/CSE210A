load ../../harness

@test "f76c9b7e88a6" {
  check 'if (¬true)    then 



x:=0   +x   else  skip     ' '⇒ skip, {}'
}
