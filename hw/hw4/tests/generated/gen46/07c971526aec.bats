load ../../harness

@test "07c971526aec" {
  check 'while 4*     -3  =  1 +   -2   do   skip    ' '⇒ skip, {}'
}
