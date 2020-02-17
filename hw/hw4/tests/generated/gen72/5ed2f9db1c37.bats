load ../../harness

@test "5ed2f9db1c37" {
  check 'if (x  *3  =   x+   4  ∨   z    -   -4  <    y -    x) then 
    skip      else  
skip     ' '⇒ skip, {}'
}
