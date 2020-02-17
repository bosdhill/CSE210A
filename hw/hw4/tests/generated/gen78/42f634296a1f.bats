load ../../harness

@test "42f634296a1f" {
  check 'if (¬(k5 < y     -    x))  then 
z   :=    W   +    y  else  skip    ' '⇒ z := (W+y), {}
⇒ skip, {z → 0}'
}
