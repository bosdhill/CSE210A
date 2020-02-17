load ../../harness

@test "8c6308b6db6c" {
  check 'if (¬(x +  z     < z     +   y))   then  skip      else   skip ' '⇒ skip, {}'
}
