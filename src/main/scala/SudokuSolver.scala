import hw.sudoku._

object Solution extends SudokuLike {

  type T = Board

  val allPairs = 0.to(8).toList.map(x => 0.to(8).toList.map(y => (x,y))).flatten.sortBy(i => i._1)

  //takes in a string and converts it into a map of available
  //values based on its peers
  def parse(str: String): Board = {
    val mappedVals = breakStr(str,0,0).toMap

    val mappedAvailable = mappedVals.map(t => (t._1,
      1.to(9).toList.diff(
      getUnavailable(mappedVals,peers(t._1._1,t._1._2),t._2,0))
    ))
    new Board(mappedAvailable)
  }

  //breaks down the input string and returns a list of coordinates with their
  //char, to be converted to a map
  def breakStr(str: String, pos: Int, start: Int): List[((Int,Int), Char)] = {
    if(pos >= str.length)
      Nil
    else if(start < 9 && pos >= 72)
      List((allPairs(pos),str.charAt(pos))) ::: breakStr(str,start+1,start+1)
    else
      List((allPairs(pos),str.charAt(pos))) ::: breakStr(str,pos+9,start)
  }

  //returns a list of unavailable numbers to place in a given coordinate based on its peers
  def getUnavailable(map: Map[(Int,Int), Char], lst: List[(Int,Int)], cur: Char, pos: Int): List[Int] = {

    if(cur != '.') //This coordinate is filled
      1.to(9).toList.diff(List(cur.asDigit))

    else if(pos < lst.length){ //Recurse to form list of valid numbers
      val value = map.get(lst(pos)).get
      if(value == '.')
        getUnavailable(map,lst,cur,pos+1)
      else
        List(value.asDigit) ::: getUnavailable(map,lst,cur,pos+1)
    }

    else //base case where cur == '.' and pos is exhausted
      Nil
  }

  //returns the list of keys that are a peer of the inputted key
  def peers(row: Int, col: Int): List[(Int,Int)] = {
    //peers in same row
    val sameRow = 0.to(8).toList.map(x => if(x != row)(x,col) else (-1,-1)).filter(v => v._1 >= 0)
    //peers in same col
    val sameCol = 0.to(8).toList.map(y => if(y != col)(row,y) else (-1,-1)).filter(v => v._1 >= 0)
    val blockRows = getBlock(row)
    val blockCols = getBlock(col)
    //peers in same blockt
    val sameBlock = blockRows.map(r => blockCols.map(c =>
      if((row,col) != (r,c)) (r,c) else (-1,-1))).flatten.filter(v => v._1 >= 0)

    (sameCol ++ sameRow ++ sameBlock).distinct

  }

  //gets the needed ints to create coordinates for blocks
  def getBlock(i: Int): List[Int] = {
    if(i <= 2)
      List(0,1,2)
    else if(i > 2 && i <= 5 )
      List(3,4,5)
    else
      List(6,7,8)
  }

}

// Top - left corner is (0 ,0). Bottom - right corner is (8 ,8). Feel free to
// change the fields of this class .
class Board(val available: Map[(Int,Int), List[Int]]) extends BoardLike[Board] {
  def availableValuesAt(row: Int, col: Int): List[Int] = available.getOrElse((row,col), 1.to(9).toList)

  //returns the value at a key, if that key can only be one value
  //None otherwise
  def valueAt(row: Int, col: Int): Option[Int] = {
    availableValuesAt(row,col) match{
      case lst if(lst.size == 1) => Some(lst(0))
      case _ => None
    }
  }
  //returns true if every key points to exactly one number
  def isSolved(): Boolean = available.filter(x => valueAt(x._1._1,x._1._2).size != 1).size == 0

  //returns true if there is a key that points to an empty list
  def isUnsolvable(): Boolean = available.filter(x => availableValuesAt(x._1._1,x._1._2).size == 0).size > 0

  //returns the board with a new value placed
  //removes the possibility of that value for its peers recursively
  def place(row: Int, col: Int, value: Int): Board = {
    val addedMap = this.available + Tuple2((row,col),List(value))
    val updatedMap = checkPeers(addedMap,Solution.peers(row,col),value)
    new Board(updatedMap)
  }

  //
  def checkPeers(addedMap: Map[(Int,Int),List[Int]], peers: List[(Int,Int)], value: Int): Map[(Int,Int), List[Int]] = {
    val newMap = addedMap ++ peers.map(key => removeFromPeers(key._1,key._2,addedMap.get(key._1,key._2).get,value))
    val recurNec = isMoreComplete(newMap,addedMap)
    if(recurNec._1){
      val a = recurNec._2
      //println(s"$a")
      multipleNews(newMap, recurNec._2.size,recurNec._2)
    }else
      newMap
  }

  //returns a recursively updated map that deletes impossible moves
  def multipleNews(m: Map[(Int,Int),List[Int]], size: Int, pairs: List[(Int,Int)]): Map[(Int,Int),List[Int]] = {
    if(size <= 0 || new Board(m).isUnsolvable())
      m
    else{
      val cur = pairs(size-1)
      val index = size-1
      val curVals = m.get(cur).get
      multipleNews(checkPeers(m,Solution.peers(cur._1,cur._2),m.get(cur).get(0)), size-1,pairs)
    }
  }

  def removeFromPeers(key1: Int, key2: Int, values: List[Int], delete: Int): ((Int,Int),List[Int]) = {
    val newVals = values.diff(List(delete))
    ((key1,key2),newVals)
  }

  //returns true if m1 is more complete than m2
  def isMoreComplete(m1: Map[(Int,Int),List[Int]], m2: Map[(Int,Int),List[Int]]): (Boolean,List[(Int,Int)]) = {
    val m1S = m1.filter(t => t._2.size == 1)
    val m2S = m2.filter(t => t._2.size == 1)
    (m1S.size > m2S.size, m1S.keySet.toList.diff(m2S.keySet.toList))
  }

  // You can return any Iterable (e.g., List )
  def nextStates(): List[Board] = {
    if(isUnsolvable())
      Nil
    else {
      val notSolved = available.filter(t => valueAt(t._1._1,t._1._2) == None)
      if(notSolved.size > 0)
        notSolved.view.map(t => availableValuesAt(t._1._1,t._1._2)
          .map(value => place(t._1._1,t._1._2,value))).reduce(_++_)
          .filter(b => !b.isUnsolvable).sortBy(board => takenSpots(board.available))
      else
        Nil
    }
  }

  def totalAvailableValues(m: Map[(Int,Int),List[Int]]): Int = m.map(t => if(t._2.size > 1)t._2.size else 0).sum

  def takenSpots(m: Map[(Int,Int),List[Int]]): Int = 5000 - m.filter(t => m.apply(t._1._1,t._1._2).size == 1).size

  def solve(): Option[Board] = {
    if(isSolved){
      Some(this)
    }else{
      val quickSolveSolution = nextStates match{
        case Nil => None
        case head :: tail => if(tail.size > 0) tail(0).solve else head.solve
      }
      if(quickSolveSolution == None)
        nextStates.find(board => board.solve != None) match {
          case None => None
          case Some(x) => x.solve
      }else
        quickSolveSolution
    }
  }
}
