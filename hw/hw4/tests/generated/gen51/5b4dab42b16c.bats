load ../../harness

@test "5b4dab42b16c" {
  check 'if (false∨     z -    -1  =   -2     --4)    then 
x  :=     y + z    else skip     ' '⇒ skip, {}'
}
