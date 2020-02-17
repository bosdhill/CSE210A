load ../../harness

@test "82b7a98b70db" {
  check 'if (1*     x     =     1    +     y)  then  k :=   4* x    else    skip ' '⇒ skip, {}'
}
