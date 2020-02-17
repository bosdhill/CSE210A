load ../../harness

@test "9b988178e075" {
  check 'while (¬true)     do 
 y    :=    x   *  -2' '⇒ skip, {}'
}
