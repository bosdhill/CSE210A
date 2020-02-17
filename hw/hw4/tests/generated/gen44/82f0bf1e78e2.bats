load ../../harness

@test "82f0bf1e78e2" {
  check 'if (true ∧ -1    *     z     =    y    * z)     then 
 skip   else  x    :=3     -4    ' '⇒ skip, {}'
}
