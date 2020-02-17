load ../../harness

@test "3bbf25024666" {
  check 'if (¬false)   then z:=    x     -     z  else 
  skip  ' '⇒ z := (x-z), {}
⇒ skip, {z → 0}'
}
