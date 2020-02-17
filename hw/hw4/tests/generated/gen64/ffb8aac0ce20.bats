load ../../harness

@test "ffb8aac0ce20" {
  check 'if (0 *    x<     3  +4    ∧    0     * x =4   -0)   then  
x   := Oo    *  2   else  x   :=4  -y   ' '⇒ x := (4-y), {}
⇒ skip, {x → 4}'
}
