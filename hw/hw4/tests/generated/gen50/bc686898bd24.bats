load ../../harness

@test "bc686898bd24" {
  check 'if (-4 +    -4<     z    +     1  ∨    -4+   x  <  4)  then 

skip else   
 y  :=    0     *     z   ' '⇒ skip, {}'
}
