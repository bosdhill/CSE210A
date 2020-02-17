load ../../harness

@test "c85ed41aa815" {
  check 'if (¬(0     *  y    =    x))    then  skip      else skip  ' '⇒ skip, {}'
}
