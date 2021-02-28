package provingground.chapter1

@main def first = 
    val A = "A" //constant string which itself can act as a type
    val aaa:A.type = A // aaa:A = "A" does not work
    val a = "a":"a" //As in proving ground "a":A does not compile
    val aa:"A" = "A" // aa:"A" = A does not work
    val c:'C' = 'C'
    val cc:c.type = c // in case of char and also int it will work
    val Ten:10 = 10
    val ten:Ten.type = 10

    println(A)
    println(a)
    println(A.getClass) //type of A
    println(a.getClass) //type of a
