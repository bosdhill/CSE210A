load ../../harness

@test "5afada903f0f" {
  check 'if (false   ∧2 =     x    -   y)  then  
skip    else 
 skip    ' '⇒ skip, {}'
}
