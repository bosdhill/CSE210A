load ../../harness

@test "39970d2cc1db" {
  check 'if (4  -     -4   = x  +  x     ∨1 -  z   = y    *    -1) then    
y    :=   -2    - -3    else  skip  ' '⇒ skip, {}'
}
