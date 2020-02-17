load ../../harness

@test "7eb517eb6d60" {
  check 'if (true ∨ 0*3=     x    +   Gw)  then      if (-4 -  3  =     -3  *   4 ∧   x   +3 =   -2) then   

 E2  := x   - 4      else   z    :=  -3*    0  else  
   y   :=-3     *    4    ' '⇒ if (((-4-3)=(-3*4))∧((x+3)=-2)) then { E2 := (x-4) } else { z := (-3*0) }, {}
⇒ z := (-3*0), {}
⇒ skip, {z → 0}'
}
