load ../../harness

@test "7785b888b250" {
  check 'if (x   +3<     -4   - 2) then   Q :=  -4-  x else   
skip  ' '⇒ skip, {}'
}
