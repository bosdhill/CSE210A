load ../../harness

@test "706fe023009f" {
  check 'y   :=y  *     x     ; 
   skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
