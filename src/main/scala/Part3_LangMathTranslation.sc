val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu",
  "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five",
  "six", "seven", "eight", "nine", "ten")

//use this for checking if valid text known in both lists
val knownLang = english ::: chinese

val test1: List[String] = List("yi", "nine", "six", "ba")
val test2: List[String] = List("yi", "josh", "nine", "six", "ba")
val test3: List[String] = List()

//checks if individual is in "table"
def myMember(elem : String, aList: List[String]) : Boolean = {
  aList match {
    case Nil => false
    case listHead :: listTail => if (elem == listHead) true else myMember(elem, listTail) //chop off the head and bind it to listHead we created. chop off tail -> listTail
  }
}

//addition function
def add(aList: List[Int]) : Int = aList.foldLeft(0)(_+_)
//multiplication function
def mult(aList: List[Int]) : Int = aList.foldLeft(1)(_*_)

//filter out unrecognized
def filter(aList: List[String]) = aList.filter(x => myMember(x, knownLang))

//applies translation from text to numbers
def translate(aList : List[String]) : List[Int] = {
  aList match {
    case Nil => Nil
    case listHead :: listTail =>
      if (english.contains(listHead)) {
        english.indexOf(listHead) :: translate(listTail)
      } else {
        chinese.indexOf(listHead) :: translate(listTail)
      }
  }
}

//takes in list
//prints results
//i.e : println(elem1+ " + " +elem2+ " = " +add(elem1, elem2))
def go(input : List[String]) : String = {

  input match{
    case Nil => "Your List Is Empty."
    case `input` => {
      //apply filter, translate, mult/add
      val newList = translate(filter(input))
      val aRes = add(newList)
      val mRes = mult(newList)
      "\n Translation " +newList.mkString(" ")+
      " \n Addition " +newList.mkString(" + ")+
      " = " +aRes+
      " \n Multiplication " +newList.mkString(" * ")+
      " = " +mRes
    }

  }
}

go(test1)
go(test2)
go(test3)