// 1. Prime Numbers:
//If p is prime, a^(p-1) = 1 (mod p) for all a in {1,..,p-1}
def prime(p : Int) : Boolean = {
  p match {
    case p if (p < 2) => false
    case p => Range(2, p-1) forall (p % _ != 0) //check if holds for each element in range
  }
}
//prime(7)
//---------------------------------------------------------------------------------------

//---------------------------------------------------------------------------------------
//2. Twin Primes
//a twin prime is a prime number that differs from another
//prime number by two
def twinPrimes(thingOne : Int, thingTwo: Int) : Boolean = {
  if(prime(thingOne) && prime(thingTwo)){
    thingTwo match{
      case 0 => return false
      case thingTwo => if(thingTwo == thingOne+2 || thingTwo == thingOne-2) return true
    }
  }
  return false
}
//twinPrimes(41,43)
//--------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------
//3. Twin Primes List
//.zip will create a list of tuples where the second element is two away from the first.
//we apply a .filter to each tuple using twinPrimes()
//.flatMap will make our list of pairs into a normal list.
//finally since we will get duplicates this way, we apply .distinct
def twinPrimesList(n : Int) : List[Int] = {
  val pList = Range(2, n).toList
  pList.zip(pList.tail.tail).filter{case (a, b) => twinPrimes(a, b)}
    .flatMap(l => List(l._1, l._2)).distinct
}

//twinPrimesList(50)
//--------------------------------------------------------------------------------------

//4. Goldbach's Conjecture--------------------------------------------------------------
//this helper method takes in two paramters, n and the created list of primes.
//and pattern matches to check:
//if the diff of n-listHead is prime, then valid and print the statement
//else difference not valid and recursive call goldList on listTail.
def goldList(n : Int, pList : List[Int]) : Unit = {
  pList match {
    case Nil => println("List is empty.")
    case listHead :: listTail => if(prime(n-listHead)) println((n-listHead)+ " + " +listHead+ " = " +n) else goldList(n, listTail)
  }
}
//does the initial checking of the entered parameter,
//creates a list of primes leading up to one less than the entered number
//calls goldList on n and the newly created list
def goldbach(n : Int) : Unit = {
  n match{
    case 2 => print("Please enter a positive even number > 2.")
    case n => {
      if(n > 2 && n%2==0)
        {
          val nList = Range(2, n-1).toList.filter(x => prime(x))
          goldList(n, nList)
        }
      else
        println("Number entered must be a positive even number greater than 2.")
    }
  }
}

goldbach(28)
//-------------------------------------------------------------------------------------

