load ../../harness

@test "e0678f8535fc" {
  check 'while y *0     =    z   -     r2   ∧     false      do    y    :=  -1 * z ' '⇒ skip, {}'
}
