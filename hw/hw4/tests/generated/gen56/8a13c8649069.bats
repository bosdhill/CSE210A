load ../../harness

@test "8a13c8649069" {
  check 'if (¬(4  +x    <    y *  2))  then 
y :=-4     -  -3  else  skip  ' '⇒ y := (-4--3), {}
⇒ skip, {y → -1}'
}
