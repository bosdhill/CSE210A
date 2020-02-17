load ../../harness

@test "0165d2362efa" {
  check 'if (x     +  y=   -1+   0  ∨    true) then  
  Q   :=   -3  -x else  y :=z    *     z   ' '⇒ Q := (-3-x), {}
⇒ skip, {Q → -3}'
}
