load ../../harness

@test "2b3281019f65" {
  check 'if (x +    z   =  1   + 0     ∧   -3   +  x =  -2    +     y)   then 
skip else z    :=     -1     * x    ' '⇒ z := (-1*x), {}
⇒ skip, {z → 0}'
}
