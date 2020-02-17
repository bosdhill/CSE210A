load ../../harness

@test "735680d5541d" {
  check 'while 4 *  -1=x  -  y  do   
x:=    -1 --2' '⇒ skip, {}'
}
