load ../../harness

@test "13e3803861a5" {
  check 'while 0  *4    =y   +   z ∧   false    do  
 y:=    y   * y ' '⇒ skip, {}'
}
