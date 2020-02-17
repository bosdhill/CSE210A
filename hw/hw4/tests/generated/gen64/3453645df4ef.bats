load ../../harness

@test "3453645df4ef" {
  check 'if (y +    4   <    y     +   x    ∨    x   *-3   =  gE)  then  x    :=  -2 + 4 else 
 
  y:=     z   *y     ' '⇒ x := (-2+4), {}
⇒ skip, {x → 2}'
}
