load ../../harness

@test "1b9d52bc66ee" {
  check 'if (¬true)      then   
skip      else  z :=   -2 *  p2' '⇒ z := (-2*p2), {}
⇒ skip, {z → 0}'
}
