load ../../harness

@test "cabbd246c307" {
  check 'if (-2 *  z=  2     *0 ∧   Z *   y  < 4    -   y)      then 
x     := y+x      else  
  skip     ' '⇒ x := (y+x), {}
⇒ skip, {x → 0}'
}
