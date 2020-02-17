load ../../harness

@test "f2bef0f7acbe" {
  check 'if (false     ∨ false)  then  x :=  y    * 0 else 
  skip    ' '⇒ skip, {}'
}
