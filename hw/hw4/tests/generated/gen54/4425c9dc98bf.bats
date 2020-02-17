load ../../harness

@test "4425c9dc98bf" {
  check 'y  :=  x-2; 
 skip' '⇒ skip; skip, {y → -2}
⇒ skip, {y → -2}'
}
