load ../../harness

@test "316b9e4dac29" {
  check 'while false∨g2     +y     <    -1   +  x do z   :=z    -     1    ' '⇒ skip, {}'
}
