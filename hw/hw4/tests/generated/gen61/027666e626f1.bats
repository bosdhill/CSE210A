load ../../harness

@test "027666e626f1" {
  check 'if (y     +    x     =   q∧   true)    then 
 DR  :=  -4+     z    else   x    :=     x*   z    ' '⇒ DR := (-4+z), {}
⇒ skip, {DR → -4}'
}
