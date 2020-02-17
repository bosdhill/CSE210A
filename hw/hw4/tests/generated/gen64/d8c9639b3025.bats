load ../../harness

@test "d8c9639b3025" {
  check 'if (true     ∨    x  - 0  <   -4   -     -1)    then 
 skip   else skip    ' '⇒ skip, {}'
}
