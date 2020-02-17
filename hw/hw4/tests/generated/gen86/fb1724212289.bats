load ../../harness

@test "fb1724212289" {
  check 'while y    =    2  -    z  ∧   false do 
 z   :=4  * z' '⇒ skip, {}'
}
