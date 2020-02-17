load ../../harness

@test "cba696a16492" {
  check 'if (¬false)  then Q     :=    z+     4  else    x    :=  -2     +  -3  ' '⇒ Q := (z+4), {}
⇒ skip, {Q → 4}'
}
