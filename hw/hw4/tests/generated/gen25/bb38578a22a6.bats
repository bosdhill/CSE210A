load ../../harness

@test "bb38578a22a6" {
  check 'if (true   ∨   true) then  skip  else  

  skip     ' '⇒ skip, {}'
}
