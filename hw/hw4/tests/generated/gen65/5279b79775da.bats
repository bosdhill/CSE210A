load ../../harness

@test "5279b79775da" {
  check 'if (3 -2    <    0 -y    ∧     y=     1)  then  
y  :=y   *-4   else   
 skip' '⇒ skip, {}'
}
