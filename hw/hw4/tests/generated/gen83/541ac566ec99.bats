load ../../harness

@test "541ac566ec99" {
  check 'while (¬¬false)    do 
y:=   x    +     2 ' '⇒ skip, {}'
}
