load ../../harness

@test "f65c1beaab54" {
  check 'if (z   *y     =     g     -   y    ∨    z<   2   *     -4)   then   
m   :=   2+y  else  y :=   1*x ' '⇒ m := (2+y), {}
⇒ skip, {m → 2}'
}
