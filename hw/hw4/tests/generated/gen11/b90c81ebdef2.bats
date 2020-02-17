load ../../harness

@test "b90c81ebdef2" {
  check 'if (0    <x  ∧   false)   then  
x :=3    +     y      else  
 skip  ' '⇒ skip, {}'
}
