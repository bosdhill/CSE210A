load ../../harness

@test "133349d67285" {
  check 'if (false ∨true) then 


skip  else y    :=    z  + -4     ' '⇒ skip, {}'
}
