load ../../harness

@test "46f509fc21e2" {
  check 'while false∨   Z9     =    x     do 
 
x :=    -3     -    3    ' '⇒ x := (-3-3); while (false∨(Z9=x)) do { x := (-3-3) }, {}
⇒ skip; while (false∨(Z9=x)) do { x := (-3-3) }, {x → -6}
⇒ while (false∨(Z9=x)) do { x := (-3-3) }, {x → -6}
⇒ skip, {x → -6}'
}
