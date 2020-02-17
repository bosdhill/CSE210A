load ../../harness

@test "5b1a00d1b506" {
  check 'if (true∨z <    -3   +   2)      then 
  z:=   2     *   x else skip     ' '⇒ z := (2*x), {}
⇒ skip, {z → 0}'
}
