load ../../harness

@test "3afed50d148d" {
  check 'if (¬(3  -    4<     z-x))   then 
skip    else  
z     :=y-   z' '⇒ z := (y-z), {}
⇒ skip, {z → 0}'
}
