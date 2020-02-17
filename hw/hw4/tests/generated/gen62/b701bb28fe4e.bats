load ../../harness

@test "b701bb28fe4e" {
  check 'while (¬true)   do     
z     :=  y   -  -1     ' '⇒ skip, {}'
}
