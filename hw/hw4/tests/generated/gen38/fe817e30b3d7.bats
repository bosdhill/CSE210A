load ../../harness

@test "fe817e30b3d7" {
  check 'if (true     ∨    V    +     1    =-1    + 2)    then  
x:=y+ x   else 
 
 y     := -3     +  3  ' '⇒ x := (y+x), {}
⇒ skip, {x → 0}'
}
