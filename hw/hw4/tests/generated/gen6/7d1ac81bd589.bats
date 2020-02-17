load ../../harness

@test "7d1ac81bd589" {
  check 'if (true∧     x=   1* x)     then  skip else skip    ' '⇒ skip, {}'
}
