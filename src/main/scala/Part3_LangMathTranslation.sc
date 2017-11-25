val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu",
  "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five",
  "six", "seven", "eight", "nine", "ten")

val test1: List[String] = List("yi", "nine", "six", "ba")
val test2: List[String] = List("yi", "josh", "nine", "six", "ba")

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
def filter(aList: List[String]) = aList.filter(x => myMember(x, chinese) || myMember(x, english))

//applies translation from text to numbers
def translate(aList : List[String]) : List[Int] = aList.map{
  case "ling" => 0
  case "zero" => 0
  case "yi" => 1
  case "one" => 1
  case "er" => 2
  case "two" => 2
  case "san" => 3
  case "three" => 3
  case "si" => 4
  case "four" =>4
  case "wu" => 5
  case "five" => 5
  case "liu" => 6
  case "six" => 6
  case "qi" => 7
  case "seven" => 7
  case "ba" => 8
  case "eight" => 8
  case "jiu" => 9
  case "nine" => 9
  case "shi" => 10
  case "ten" => 10
}

//takes in list
//prints results
//i.e : println(elem1+ " + " +elem2+ " = " +add(elem1, elem2))
def go(input : List[String]) : String = {
  input match{
    case Nil => "Empty List"
    case input => {

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