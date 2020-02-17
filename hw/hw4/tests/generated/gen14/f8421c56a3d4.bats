load ../../harness

@test "f8421c56a3d4" {
  check 'if (false    ∧   x  * -3     =    z  *-1)     then skip    else   
{skip   ;

y   := 2}   ' '⇒ skip; y := 2, {}
⇒ y := 2, {}
⇒ skip, {y → 2}'
}
