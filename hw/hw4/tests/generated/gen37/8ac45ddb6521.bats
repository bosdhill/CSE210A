load ../../harness

@test "8ac45ddb6521" {
  check 'if (-2 - qA=    z    +     y    ∨  true)  then 
   z :=2+   x     else z   :=   -1  -    -3' '⇒ z := (2+x), {}
⇒ skip, {z → 2}'
}
