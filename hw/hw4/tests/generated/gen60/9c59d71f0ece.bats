load ../../harness

@test "9c59d71f0ece" {
  check 'if (1   *     4    <     1   +    z     ∨    -2 +   vG  = -4     *    -4)    then 

x:= 0* 1   else x     :=     -2     -    x  ' '⇒ x := (-2-x), {}
⇒ skip, {x → -2}'
}
