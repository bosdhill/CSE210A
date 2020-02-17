load ../../harness

@test "baf9e8d2547b" {
  check 'if (1    - -2    =   1-   MX   ∧0-   -1   =     z+     z)     then y   :=3 --2  else  z   :=   -1    *  0    ' '⇒ z := (-1*0), {}
⇒ skip, {z → 0}'
}
