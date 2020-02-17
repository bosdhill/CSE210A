load ../../harness

@test "542276a12fce" {
  check 'if (4  <z   ∧  false)      then skip    else 

skip' '⇒ skip, {}'
}
