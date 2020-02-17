load ../../harness

@test "3c0d43c13936" {
  check 'if (¬(x   +   -4     <x     * 2)) then 

z    :=    -1   +x    else   z    :=    -3     +   x  ' '⇒ z := (-3+x), {}
⇒ skip, {z → -3}'
}
