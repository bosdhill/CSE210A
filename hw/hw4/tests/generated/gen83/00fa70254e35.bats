load ../../harness

@test "00fa70254e35" {
  check 'while 3 -     x =1    -    3 ∧    true   do  

 z     := x   -    -1' '⇒ skip, {}'
}
