load ../../harness

@test "81145f1be0e2" {
  check 'if (true∧     true)    then 
skip else 
  x  :=  z +   3' '⇒ skip, {}'
}
