load ../../harness

@test "b4278b34d328" {
  check 'if (¬(y    --1    <-2))     then  

skip     else   

y :=     2     +    1 ' '⇒ skip, {}'
}
