load ../../harness

@test "515e744f8b62" {
  check 'if (¬(1     *    -2  =  y  +     1))   then  
z:=-2    *y   else z:= 4     *   -2   ' '⇒ z := (-2*y), {}
⇒ skip, {z → 0}'
}
