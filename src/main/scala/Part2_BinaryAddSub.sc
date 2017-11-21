// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: <Quinton Outten>


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1)
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)

val pTestMy: List[Int] = List(1, 0, 0, 1, 0, 0, 1)
val qTestMy: List[Int] = List(0, 0, 1, 1, 0, 0, 1)
val testMySolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = ???

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  carryBit match {
      //if carryBit false and rest are true
    case false => if(pBit && qBit) true else false
      //case all 3 are true
    case true => if(pBit && qBit && carryBit) true else false
  }
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  qBit match {
    case false => if(!pBit && !getNextCarryBit(qBit, !pBit, carryBit)) false else true //if(0+0) false else(if 1+0) true
    case true => if(!pBit) true else getNextCarryBit(pBit, qBit, carryBit)//if(0+1) else (1+1)
  }
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  if(pBits.length == qBits.length){
    pBits zip qBits map {case(a, b) => addBits(a, b, carryBit)}

  } else {
    pBits zip qBits map {case(a, b) => addBits(a, b, carryBit)}
    //finishBinaryAdd???
  }
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) : List[Boolean] = intList.map{
  case 0 => false
  case 1 => true
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) : List[Int] = booleanList.map{
  case false => 0
  case true => 1
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean.
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  val pBits = convertIntListToBooleanList(pList).reverse
  val qBits = convertIntListToBooleanList(qList).reverse
  val carry = false
  val sol = doBinaryAddition(pBits, qBits, carry)
  convertBooleanListToIntList(sol).reverse
}


// def bindarySubtraction(pList: List[Int], qList: List[Int]) = ???
/*
Binary add works, but not with carry bits!!!******************************************
 */
if (binaryAddition(pTestMy, qTestMy).equals(testMySolution)) println("Test 1 passes!") else println("Test 1 fails.")

// Testing binary addition.
//if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
//if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
//if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
//if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
//if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
//if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")

