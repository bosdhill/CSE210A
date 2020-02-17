load ../../harness

@test "b7c14c2cb331" {
  check 'x   :=     2   *   -3;  
 skip' '⇒ skip; skip, {x → -6}
⇒ skip, {x → -6}'
}
