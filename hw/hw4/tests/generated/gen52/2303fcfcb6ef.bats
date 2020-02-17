load ../../harness

@test "2303fcfcb6ef" {
  check 'if (true     ∨    x   +x<    2 + -1)  then skip      else  
 
 x     := z     +   0' '⇒ skip, {}'
}
