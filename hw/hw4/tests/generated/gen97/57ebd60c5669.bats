load ../../harness

@test "57ebd60c5669" {
  check 'if (¬true)      then   skip    else ks :=     x  -  x     ' '⇒ ks := (x-x), {}
⇒ skip, {ks → 0}'
}
