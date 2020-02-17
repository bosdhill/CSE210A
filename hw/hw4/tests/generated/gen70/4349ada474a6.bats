load ../../harness

@test "4349ada474a6" {
  check 'if (false     ∨    X8 <     4   +     -1)    then 


 skip    else skip' '⇒ skip, {}'
}
