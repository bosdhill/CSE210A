load ../../harness

@test "f8b1e759dfcc" {
  check 'if (¬(x   *   z     =  -2   - z))    then  
skip      else  

z   :=     z   +  3  ' '⇒ skip, {}'
}
