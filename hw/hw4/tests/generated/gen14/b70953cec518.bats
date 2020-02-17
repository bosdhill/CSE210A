load ../../harness

@test "b70953cec518" {
  check 'while 3  -   4=    z     +z     do x:= z     *     y' '⇒ skip, {}'
}
