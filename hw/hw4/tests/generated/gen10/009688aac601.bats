load ../../harness

@test "009688aac601" {
  check 'if (-1     +  0 <    y * -2     ∧   z   +     -4   =  x  -     z)     then 
 x :=  x  else 
 skip  ' '⇒ skip, {}'
}
