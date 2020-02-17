load ../../harness

@test "b90db2b1689d" {
  check 'if (x+     x   =    0 +    y∨     false)  then  w   :=z    -  -2 else  
K    :=   -2  ' '⇒ w := (z--2), {}
⇒ skip, {w → 2}'
}
