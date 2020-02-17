load ../../harness

@test "bc8a1bb65050" {
  check 'if (true     ∨false)    then 
z   :=     4    -  y      else skip     ' '⇒ z := (4-y), {}
⇒ skip, {z → 4}'
}
