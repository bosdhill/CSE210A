load ../../harness

@test "05b6d7b5c184" {
  check 'if (false   ∨   true)  then 

skip  else  skip ' '⇒ skip, {}'
}
