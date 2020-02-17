load ../../harness

@test "56f8d8c8dc13" {
  check 'x   :=    -3   *    y  ;
 
x     := 0    * y' '⇒ skip; x := (0*y), {x → 0}
⇒ x := (0*y), {x → 0}
⇒ skip, {x → 0}'
}
