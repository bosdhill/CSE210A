load ../../harness

@test "d877937572d1" {
  check 'if (¬(x  *     z    =2     - 1))     then  x:= y-y     else x   :=  4-    x' '⇒ x := (y-y), {}
⇒ skip, {x → 0}'
}
