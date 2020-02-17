load ../../harness

@test "4ce7e43050e9" {
  check 'if (¬(y*     x     =    y   +l))      then  
  skip   else   Ti   := y     --4' '⇒ Ti := (y--4), {}
⇒ skip, {Ti → 4}'
}
