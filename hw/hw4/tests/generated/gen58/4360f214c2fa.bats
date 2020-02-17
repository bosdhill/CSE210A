load ../../harness

@test "4360f214c2fa" {
  check 'if (x   +0  <    x  +-1 ∧     true)      then  
  
x     :=x * -2 else  skip' '⇒ skip, {}'
}
