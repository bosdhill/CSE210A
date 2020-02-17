load ../../harness

@test "b5549cdd936b" {
  check 'if (¬true)    then  
  skip      else 
 
x   :=     -2     * 1 ' '⇒ x := (-2*1), {}
⇒ skip, {x → -2}'
}
