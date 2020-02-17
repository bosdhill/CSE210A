load ../../harness

@test "fe3731553f4b" {
  check 'while 3+     2<1 -H   do 
  skip     ' '⇒ skip, {}'
}
