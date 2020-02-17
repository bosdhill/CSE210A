load ../../harness

@test "5e4cd392a3ca" {
  check 'while false∨     z-  z    = z   +    y do 
y:=  3    + oQ  ' '⇒ y := (3+oQ); while (false∨((z-z)=(z+y))) do { y := (3+oQ) }, {}
⇒ skip; while (false∨((z-z)=(z+y))) do { y := (3+oQ) }, {y → 3}
⇒ while (false∨((z-z)=(z+y))) do { y := (3+oQ) }, {y → 3}
⇒ skip, {y → 3}'
}
