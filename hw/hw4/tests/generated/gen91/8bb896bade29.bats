load ../../harness

@test "8bb896bade29" {
  check 'if (true     ∧false)    then skip    else 

skip' '⇒ skip, {}'
}
