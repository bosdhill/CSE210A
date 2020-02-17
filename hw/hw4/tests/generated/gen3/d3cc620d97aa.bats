load ../../harness

@test "d3cc620d97aa" {
  check 'if (true   ∧ 2   +     y    <    0   *    3) then  
   z :=  -2    else skip     ' '⇒ skip, {}'
}
