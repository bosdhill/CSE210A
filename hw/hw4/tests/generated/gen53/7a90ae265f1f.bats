load ../../harness

@test "7a90ae265f1f" {
  check 'if (¬true)  then  
 z  :=   -1   else   skip' '⇒ skip, {}'
}
