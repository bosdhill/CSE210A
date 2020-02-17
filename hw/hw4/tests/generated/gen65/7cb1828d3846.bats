load ../../harness

@test "7cb1828d3846" {
  check 'if (F    =     3     -   y     ∨  false) then 
 skip   else  
skip' '⇒ skip, {}'
}
