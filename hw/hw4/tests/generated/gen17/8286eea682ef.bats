load ../../harness

@test "8286eea682ef" {
  check 'z  :=    z  *0   ;  
  skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
