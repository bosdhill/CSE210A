load ../../harness

@test "f3cffb7e05d3" {
  check 'while (¬(x*     z   <3))  do     skip ' '⇒ skip, {}'
}
