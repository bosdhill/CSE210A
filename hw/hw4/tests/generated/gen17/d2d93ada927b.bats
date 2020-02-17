load ../../harness

@test "d2d93ada927b" {
  check 'y :=   -2  * z  ;
 
x    :=   x +    z     ' '⇒ skip; x := (x+z), {y → 0}
⇒ x := (x+z), {y → 0}
⇒ skip, {x → 0, y → 0}'
}
