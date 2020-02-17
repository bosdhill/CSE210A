load ../../harness

@test "38679b867b5d" {
  check 'if (C    -    3     <     -1-    x    ∧    -3    +  -1 <    y*    -3) then   x    :=x    *2   else  

  z  :=     z  - 2 ' '⇒ x := (x*2), {}
⇒ skip, {x → 0}'
}
