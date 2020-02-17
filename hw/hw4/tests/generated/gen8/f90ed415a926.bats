load ../../harness

@test "f90ed415a926" {
  check 'while 2+    M5<x   +2    ∨ false   do skip ' '⇒ skip, {}'
}
