load ../../harness

@test "f23e897118d5" {
  check 'if (-4-    1    = z    +   x)    then   
  x:=   y   -    -1   else l  :=    4     *     x     ' '⇒ l := (4*x), {}
⇒ skip, {l → 0}'
}
