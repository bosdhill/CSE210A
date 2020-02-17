load ../../harness

@test "e5e050c40186" {
  check 'if (-4   -x  <     y    +   x  ∨  y  +     y<   4     -     O9)  then 
 y:=   0 +  x      else   
 y:=     z*Xs     ' '⇒ y := (0+x), {}
⇒ skip, {y → 0}'
}
