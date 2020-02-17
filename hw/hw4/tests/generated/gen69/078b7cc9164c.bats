load ../../harness

@test "078b7cc9164c" {
  check 'if (¬(z=     x* Cl))     then 
 x:=    2+x      else   skip' '⇒ skip, {}'
}
