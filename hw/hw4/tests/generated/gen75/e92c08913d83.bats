load ../../harness

@test "e92c08913d83" {
  check 'if (false ∨    x  -x    =x+    z)    then   skip else skip  ' '⇒ skip, {}'
}
