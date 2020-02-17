load ../../harness

@test "a78a3f1b4def" {
  check 'if (2   <x-x   ∧   z   +  1     < -2) then skip   else 
    skip' '⇒ skip, {}'
}
