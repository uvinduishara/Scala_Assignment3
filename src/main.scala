object main {

  def main(args:Array[String]):Unit = {

    println(prime(23))

    primeSeq(10)

    println("")

    println(Sum(10))

    if(isEven(75)){
      println("Even")
    }
    else {
      println("Odd")
    }

    println(EvenSum(11))

    fibonacciSeq(10)



  }

  def GCD(a: Int, b: Int): Int = b match {
    case 0          => a
    case x if x > a => GCD(x, a)
    case x         => GCD(x, a % x)
  }

  def prime(p: Int, n: Int = 2): Boolean = n match {
    case x if x == p        => true
    case x if GCD(p, x) > 1 => false
    case x                  => prime(p, x + 1)
  }


  def primeSeq(n:Int):Unit={
    if (n==3) {
      print(n-1+",")
    }
    else{
      primeSeq(n-1)

      if(prime(n-1)){
        print(n-1+",")
      }
    }

  }

  def Sum(n:Int):Int={
    if (n==1) {
      1
    }
    else{
      n + Sum(n-1)
    }

  }

  def isEven(n: Int): Boolean = {
    if (n == 0)  true
    else if (n == 1)  false
    else isEven(n - 2)
  }

  def EvenSum(n:Int):Int={
    if (n==2) {
      0
    }
    else{
      if (isEven(n-1)) n-1 +EvenSum(n-1)
      else EvenSum(n-1)
    }

  }

  def fibonacci(n:Int):Int= n match{
    case x if x==0 => 0
    case x if x==1 => 1
    case x => fibonacci(n-1)+fibonacci(n-2)
  }

  def fibonacciSeq(n:Int):Unit= {
    if (n > 0) fibonacciSeq(n-1)
    print(fibonacci(n)+",")
  }



}
