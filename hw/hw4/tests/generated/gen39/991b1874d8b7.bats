load ../../harness

@test "991b1874d8b7" {
  check 'if (-3    <x    +    -4 ∧  -1*   z<  -2    +Q)     then 

y :=  2   +-3    else while y     +   z =  x    -    3    ∧   2 +  a =  -2 do   skip' '⇒ while (((y+z)=(x-3))∧((2+a)=-2)) do { skip }, {}
⇒ skip, {}'
}
