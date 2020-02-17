load ../../harness

@test "1fcf4dd91a89" {
  check 'if (y     -    -2   <   x    +   z   ∨ false)     then 
 skip     else  skip     ' '⇒ skip, {}'
}
