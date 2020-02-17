load ../../harness

@test "2a036fef66fa" {
  check 'if (x   -  z<   3    +     y)     then x   :=     z     -     2      else 
x:=  gH* x  ' '⇒ x := (z-2), {}
⇒ skip, {x → -2}'
}
