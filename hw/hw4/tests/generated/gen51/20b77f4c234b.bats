load ../../harness

@test "20b77f4c234b" {
  check 'if (true     ∨ true)    then 
x:=  y  -  -4  else  skip   ' '⇒ x := (y--4), {}
⇒ skip, {x → 4}'
}
