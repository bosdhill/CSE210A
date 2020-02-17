load ../../harness

@test "94347cb10fc0" {
  check 'if (false ∨    3  -    -4     =  y    -x)    then    
skip else    skip' '⇒ skip, {}'
}
