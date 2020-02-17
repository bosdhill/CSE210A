load ../../harness

@test "1aaa4c00a270" {
  check 'if (true∨     -2  * y   <  y   -   x)  then   skip    else  hk   :=VB *  y  ' '⇒ skip, {}'
}
