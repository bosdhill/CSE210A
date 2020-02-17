load ../../harness

@test "da2c0b663689" {
  check 'if (false∧x  +     -3 =x    -   1)  then 
z  :=    z +     z   else  
x    :=x+    z  ' '⇒ x := (x+z), {}
⇒ skip, {x → 0}'
}
