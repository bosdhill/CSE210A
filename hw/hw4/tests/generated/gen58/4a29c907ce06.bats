load ../../harness

@test "4a29c907ce06" {
  check 'if (true  ∨     true) then 
skip else 
x   :=   4    +  4    ' '⇒ skip, {}'
}
