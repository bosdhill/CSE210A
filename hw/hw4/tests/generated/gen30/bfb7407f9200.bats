load ../../harness

@test "bfb7407f9200" {
  check 'while x     +     1  =     -4  +  w ∨   z   =  1    * y    do  
 y     :=-1+   y' '⇒ y := (-1+y); while (((x+1)=(-4+w))∨(z=(1*y))) do { y := (-1+y) }, {}
⇒ skip; while (((x+1)=(-4+w))∨(z=(1*y))) do { y := (-1+y) }, {y → -1}
⇒ while (((x+1)=(-4+w))∨(z=(1*y))) do { y := (-1+y) }, {y → -1}
⇒ skip, {y → -1}'
}
