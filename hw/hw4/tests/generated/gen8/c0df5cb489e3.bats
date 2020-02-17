load ../../harness

@test "c0df5cb489e3" {
  check 'if (true∨     1    +     x   =    -4   *     y)    then 
 skip   else Vl  :=-2  ' '⇒ skip, {}'
}
