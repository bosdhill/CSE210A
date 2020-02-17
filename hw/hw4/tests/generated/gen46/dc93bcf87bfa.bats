load ../../harness

@test "dc93bcf87bfa" {
  check 'if (true  ∨-3 -     4=   2  *   x) then 
 skip      else  
 {skip ;


y  :=  y -1}     ' '⇒ skip, {}'
}
