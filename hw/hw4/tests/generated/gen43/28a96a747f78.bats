load ../../harness

@test "28a96a747f78" {
  check 'if (¬true)    then 
x  :=   2   else  z    :=   4   +  1    ' '⇒ z := (4+1), {}
⇒ skip, {z → 5}'
}
