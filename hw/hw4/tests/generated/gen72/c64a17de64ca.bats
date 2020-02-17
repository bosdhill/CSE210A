load ../../harness

@test "c64a17de64ca" {
  check 'if (false  ∨    3   +     4  <0*  1)   then skip    else 

   skip   ' '⇒ skip, {}'
}
