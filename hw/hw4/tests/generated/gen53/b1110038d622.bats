load ../../harness

@test "b1110038d622" {
  check 'if (¬(-3  -     3 =   z   -x)) then 

y  :=-4    * 0    else z     :=   z   ' '⇒ y := (-4*0), {}
⇒ skip, {y → 0}'
}
