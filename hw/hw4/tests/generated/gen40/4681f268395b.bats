load ../../harness

@test "4681f268395b" {
  check 'if (-2     +BO  =  3   -   2∧   true)  then if (¬true)   then  
y     := x+     4     else  
skip     else 
  skip  ' '⇒ skip, {}'
}
