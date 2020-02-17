load ../../harness

@test "ae5e9f37033a" {
  check 'if (x   +     -2    <    y)    then 
 skip      else   skip' '⇒ skip, {}'
}
