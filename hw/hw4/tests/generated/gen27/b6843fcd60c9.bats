load ../../harness

@test "b6843fcd60c9" {
  check 'if (true    ∧     I0  + x     =     x    -   y)   then x:=    2  * x   else  
skip   ' '⇒ x := (2*x), {}
⇒ skip, {x → 0}'
}
