load ../../harness

@test "5fe436c57236" {
  check 'if (¬(0     *    y<-4    + z))      then   z :=    y     - z    else   
skip  ' '⇒ z := (y-z), {}
⇒ skip, {z → 0}'
}
