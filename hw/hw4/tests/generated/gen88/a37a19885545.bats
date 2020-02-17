load ../../harness

@test "a37a19885545" {
  check 'if (true    ∧false)  then  
 x:=     3 *    -3  else skip ' '⇒ skip, {}'
}
