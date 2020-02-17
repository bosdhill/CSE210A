load ../../harness

@test "1315d12940c6" {
  check 'if (x    *-2=    2  --4    ∨false)    then  
x  :=     x     -y   else  skip   ' '⇒ skip, {}'
}
