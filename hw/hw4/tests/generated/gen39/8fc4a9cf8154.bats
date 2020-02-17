load ../../harness

@test "8fc4a9cf8154" {
  check 'if (y    +     -1     <     -2    *     z    ∨  false)  then   z     :=    y+   2    else  
y :=    0 ' '⇒ z := (y+2), {}
⇒ skip, {z → 2}'
}
