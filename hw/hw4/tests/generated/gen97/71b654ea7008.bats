load ../../harness

@test "71b654ea7008" {
  check 'if (false     ∨   o   *    x=   z *x)    then  skip      else  Fz   :=     z     +64407812832129714553946137727225463793 ' '⇒ skip, {}'
}
