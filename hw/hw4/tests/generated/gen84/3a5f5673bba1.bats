load ../../harness

@test "3a5f5673bba1" {
  check 'if (-2  < z  -     y   ∧ -2 +  x<4    -   -2)    then y:=  -1    +1      else  
   skip  ' '⇒ y := (-1+1), {}
⇒ skip, {y → 0}'
}
