load ../../harness

@test "fe4d6edb12b4" {
  check 'if (-1     *   z     =  3     -   z     ∧   false) then 
 

 skip  else  
 z    :=    x    -    -3 ' '⇒ z := (x--3), {}
⇒ skip, {z → 3}'
}
