load ../../harness

@test "c1053709de30" {
  check 'if (4  -y     =  N6     +   y  ∨  true)     then 
    y:=  -4 *   -3    else  bl:=   -3    *   -3     ' '⇒ y := (-4*-3), {}
⇒ skip, {y → 12}'
}
