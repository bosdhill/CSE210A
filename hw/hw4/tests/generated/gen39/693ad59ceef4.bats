load ../../harness

@test "693ad59ceef4" {
  check 'if (¬false)    then 

skip      else  x    :=    2    ' '⇒ skip, {}'
}
