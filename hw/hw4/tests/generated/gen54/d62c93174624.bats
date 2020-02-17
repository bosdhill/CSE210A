load ../../harness

@test "d62c93174624" {
  check 'if (true     ∨x  - -2     =4  + 4)    then skip else  
x:=   0   -    -4     ' '⇒ skip, {}'
}
