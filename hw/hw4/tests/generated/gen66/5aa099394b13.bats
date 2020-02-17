load ../../harness

@test "5aa099394b13" {
  check 'if (-1+    z     =y-   4)   then  
   skip     else z     :=     0   +-4  ' '⇒ z := (0+-4), {}
⇒ skip, {z → -4}'
}
