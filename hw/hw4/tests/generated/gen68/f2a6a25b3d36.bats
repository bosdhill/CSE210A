load ../../harness

@test "f2a6a25b3d36" {
  check 'if (¬(3  *  3 <    I +  3))  then   
z    := x -   -4      else  skip     ' '⇒ z := (x--4), {}
⇒ skip, {z → 4}'
}
