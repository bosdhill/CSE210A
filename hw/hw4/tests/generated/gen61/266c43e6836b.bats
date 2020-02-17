load ../../harness

@test "266c43e6836b" {
  check 'if (x     * t <   y  ∨   -1    +   z = -1    -  y)  then k     :=   0 +  -3    else   y  :=0 ' '⇒ k := (0+-3), {}
⇒ skip, {k → -3}'
}
