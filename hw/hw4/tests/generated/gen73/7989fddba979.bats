load ../../harness

@test "7989fddba979" {
  check 'if (-1    *  4     =   0     +     x∨  y  -     3   <     1     -    -1)   then 
  skip     else    skip  ' '⇒ skip, {}'
}
