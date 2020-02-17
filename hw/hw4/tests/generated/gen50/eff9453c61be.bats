load ../../harness

@test "eff9453c61be" {
  check 'if (z    * z     <   z -   2    ∨   -3  +    T    < -4*2)     then 
y  :=    z     *3    else skip     ' '⇒ skip, {}'
}
