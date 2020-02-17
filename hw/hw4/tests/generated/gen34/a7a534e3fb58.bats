load ../../harness

@test "a7a534e3fb58" {
  check 'if (¬true)  then 
 y  := 4     +  -1   else   
z     :=    x   -    x    ' '⇒ z := (x-x), {}
⇒ skip, {z → 0}'
}
