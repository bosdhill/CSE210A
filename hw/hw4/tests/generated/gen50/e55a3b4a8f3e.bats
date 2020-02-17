load ../../harness

@test "e55a3b4a8f3e" {
  check 'if (¬(-3-x    <    T*    -1)) then skip else  z:=   3-  z   ' '⇒ z := (3-z), {}
⇒ skip, {z → 3}'
}
