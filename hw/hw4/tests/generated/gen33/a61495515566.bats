load ../../harness

@test "a61495515566" {
  check 'if (x   -z =   1 ∨    -3   -1     =     -4   *     -1)  then x   :=  -4     - 0     else 
z   :=    y    --4     ' '⇒ z := (y--4), {}
⇒ skip, {z → 4}'
}
