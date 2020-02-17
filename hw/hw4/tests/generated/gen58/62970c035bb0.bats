load ../../harness

@test "62970c035bb0" {
  check 'while false    ∨     false   do   Z2    :=  x   -     y     ' '⇒ skip, {}'
}
