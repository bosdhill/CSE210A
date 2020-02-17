load ../../harness

@test "28602889e1f2" {
  check 'if (¬true)      then 
y     :=   -4  else  
skip' '⇒ skip, {}'
}
