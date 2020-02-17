load ../../harness

@test "76d9fa56fccb" {
  check 'while 1    +    z   =    y   *   z   ∧   false    do 



 y :=   4  *  -4' '⇒ skip, {}'
}
