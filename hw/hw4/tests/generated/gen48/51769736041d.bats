load ../../harness

@test "51769736041d" {
  check 'while 1*   1   =     y -     z     ∧    0   *     z  <z   -     y do   
z  :=   z-     -2  ' '⇒ skip, {}'
}
