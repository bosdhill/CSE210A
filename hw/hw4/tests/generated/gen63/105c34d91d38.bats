load ../../harness

@test "105c34d91d38" {
  check 'while x< 4    +  2     ∧     it   +   1<  x  +1    do x   :=  z    ' '⇒ skip, {}'
}
