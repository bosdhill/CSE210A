load ../../harness

@test "4da4b3875754" {
  check 'if (false    ∨ -4     -   x    =  x     * 1)   then 

x    := -4-  c  else    l     :=     y*   -4  ' '⇒ l := (y*-4), {}
⇒ skip, {l → 0}'
}
