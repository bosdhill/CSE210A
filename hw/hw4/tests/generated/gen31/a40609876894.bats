load ../../harness

@test "a40609876894" {
  check 'if (0*     x  =f     - y  ∨ z   *y   =   x -    -3)    then skip      else     
skip    ' '⇒ skip, {}'
}
