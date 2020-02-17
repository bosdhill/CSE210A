load ../../harness

@test "6518117a9e4b" {
  check 'while (¬true)    do  
b8     :=    y  -   a ' '⇒ skip, {}'
}
