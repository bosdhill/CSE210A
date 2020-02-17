load ../../harness

@test "291a9d3bb506" {
  check 'while 1 +     -4   =  2   + y   ∧    x - 4     <     z do  y    :=    z  + 4     ' '⇒ skip, {}'
}
