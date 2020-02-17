load ../../harness

@test "4336028ab37a" {
  check 'if (¬(2     -    1  =-3    -  L))  then    skip    else  x   :=    p     +    4' '⇒ skip, {}'
}
