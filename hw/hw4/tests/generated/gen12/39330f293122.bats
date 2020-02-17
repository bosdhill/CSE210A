load ../../harness

@test "39330f293122" {
  check 'z   :=  -4  *     x   ;

 

skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
