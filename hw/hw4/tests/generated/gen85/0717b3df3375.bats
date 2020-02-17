load ../../harness

@test "0717b3df3375" {
  check 'skip  ;  r   :=a   + x' '⇒ r := (a+x), {}
⇒ skip, {r → 0}'
}
