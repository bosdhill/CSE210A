load ../../harness

@test "c6366c3767eb" {
  check 'if (¬(-4 -     3     =  x     -  x))      then 
  skip    else    W:=   y  -  x     ' '⇒ skip, {}'
}
