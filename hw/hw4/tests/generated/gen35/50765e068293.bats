load ../../harness

@test "50765e068293" {
  check 'if (x     -y <    2   +    1)     then 
I2  :=     z *     y     else 
  skip   ' '⇒ I2 := (z*y), {}
⇒ skip, {I2 → 0}'
}
