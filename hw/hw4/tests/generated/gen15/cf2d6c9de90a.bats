load ../../harness

@test "cf2d6c9de90a" {
  check 'if (false    ∨ false)      then  x    :=1- x   else skip   ' '⇒ skip, {}'
}
