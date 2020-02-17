load ../../harness

@test "67c565486c02" {
  check 'if (¬(x -    3  <    x *  -4))      then 
skip else skip     ' '⇒ skip, {}'
}
