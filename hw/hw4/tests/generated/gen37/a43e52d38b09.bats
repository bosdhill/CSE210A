load ../../harness

@test "a43e52d38b09" {
  check 'if (false     ∨   N  +     -4    <     1   +   -3)   then  x :=  1    * -3    else  skip   ' '⇒ x := (1*-3), {}
⇒ skip, {x → -3}'
}
