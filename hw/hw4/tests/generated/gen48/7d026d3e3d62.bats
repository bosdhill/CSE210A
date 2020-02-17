load ../../harness

@test "7d026d3e3d62" {
  check 'if (¬(2    - 3  <  -3 -    -3))     then skip      else skip    ' '⇒ skip, {}'
}
