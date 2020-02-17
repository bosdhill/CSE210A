load ../../harness

@test "42cd0f924891" {
  check 'y   :=  x +x   ;
 
z :=   y*  z    ' '⇒ skip; z := (y*z), {y → 0}
⇒ z := (y*z), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
