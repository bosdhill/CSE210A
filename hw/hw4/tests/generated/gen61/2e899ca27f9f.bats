load ../../harness

@test "2e899ca27f9f" {
  check 'if (¬(-2 +     1    =  -3  +y))     then   skip      else 
y :=  1*  -1   ' '⇒ skip, {}'
}
