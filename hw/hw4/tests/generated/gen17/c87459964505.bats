load ../../harness

@test "c87459964505" {
  check 'if (-2   -     2     <x  +  z  ∧ -1 *   y   =    0*    -1)  then    
y    :=   -3 *    (-4    +  x)    else  skip   ' '⇒ y := (-3*(-4+x)), {}
⇒ skip, {y → 12}'
}
