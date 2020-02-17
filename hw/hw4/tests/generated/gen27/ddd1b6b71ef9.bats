load ../../harness

@test "ddd1b6b71ef9" {
  check 'if (¬(x     -  x   =    y* y))    then   skip   else     zR    :=  2     *     (1   +     -2)    ' '⇒ zR := (2*(1+-2)), {}
⇒ skip, {zR → -2}'
}
