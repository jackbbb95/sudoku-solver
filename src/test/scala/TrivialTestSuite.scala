import Solution._

class TrivialTestSuite extends org.scalatest.FunSuite {
  test ("The solution object must be defined") {
    val obj: hw.sudoku.SudokuLike = Solution
  }

  test("Basic Solution Tests") {
    assert(peers(0,0) == List((0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7),
                                   (0,8), (1,0), (2,0), (3,0), (4,0), (5,0),
                                   (6,0), (7,0), (8,0), (1,1), (1,2), (2,1), (2,2))
                                   && !peers(0,0).contains((0,0)))

    assert(peers(5,4) == List((5,0), (5,1), (5,2), (5,3), (5,5), (5,6), (5,7),
                              (5,8), (0,4), (1,4), (2,4), (3,4), (4,4),
                              (6,4), (7,4), (8,4), (3,3), (3,5), (4,3), (4,5))
                              && !peers(5,4).contains((5,4)))

    assert(allPairs == List((0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), (0,8),
                            (1,0), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7), (1,8),
                            (2,0), (2,1), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7), (2,8),
                            (3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7), (3,8),
                            (4,0), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7), (4,8),
                            (5,0), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6), (5,7), (5,8),
                            (6,0), (6,1), (6,2), (6,3), (6,4), (6,5), (6,6), (6,7), (6,8),
                            (7,0), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6), (7,7), (7,8),
                            (8,0), (8,1), (8,2), (8,3), (8,4), (8,5), (8,6), (8,7), (8,8)))

  }

  test("Place Test"){
    val board = parse(".................................................................................")
    println(s"ORIGINAL: \n$board")
    val n = board.place(0,0,1).place(1,0,2).place(2,0,3).place(3,0,4).place(4,0,5).place(5,0,6).place(6,0,7).place(7,0,8).place(8,0,9)
    println(s"PLACED: \n$n")
  }

  test("NextStates Test"){
    val board = parse("41736982......89479587243168254......91586432346912758....43571573291684164875293")
    assert(!board.isSolved)
    println(s"ORIGINAL: \n$board")
    val n = board.nextStates
    println(s"NextState: ")
    n.map(b => println(s"$b"))
  }

  test("ValueAt Test"){
    val board = parse("417369825632158947958724316825437169791586432346912758289643571573291684164875293")
    assert(board.isSolved)
    val b = parse("41736982......89479587243168254......91586432346912758....43571573291684164875293")
    assert(b.valueAt(1,0) == None)
    assert(b.valueAt(0,0) == Some(4))
  }

  test("Board Test 1") {
    val board = parse(
      "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
    )
    println(s"ORIGINAL BOARD: \n$board")
    assert(board.valueAt(1,1) == Some(6))
    assert(board.valueAt(4,4) == None)
    assert(!board.isSolved)
    assert(!board.isUnsolvable)

    val newBoard = board.solve.get
    println(s"SOLUTION: \n$newBoard")

  }

  test("Board Test 2") {
    val board = parse(
      "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
    )
    println(s"ORIGINAL BOARD: \n$board")
    assert(board.valueAt(0,0) == Some(8))
    assert(board.valueAt(0,2) == None)
    assert(!board.isSolved)
    assert(!board.isUnsolvable)

    val newBoard = board.solve.get
    println(s"SOLUTION: \n$newBoard")
  }

  test("Board Test 3") {
    val board = parse(
      ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
    )
    println(s"ORIGINAL BOARD: \n$board")
    assert(board.valueAt(0,0) == None)
    assert(board.valueAt(0,1) == Some(1))
    assert(!board.isSolved)
    assert(!board.isUnsolvable)

    val newBoard = board.solve.get
    println(s"SOLUTION: \n$newBoard")
  }

  test("Board Test 4") {
    val board = parse(
      ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
    )
    println(s"ORIGINAL BOARD: \n$board")
    assert(board.valueAt(0,0) == None)
    assert(board.valueAt(0,1) == Some(4))
    assert(!board.isSolved)
    assert(!board.isUnsolvable)

    val newBoard = board.solve.get
    println(s"SOLUTION: \n$newBoard")
  }

  test("Board Test 5") {
    val board = parse(
      "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
    )
    println(s"ORIGINAL BOARD: \n$board")
    assert(board.valueAt(0,0) == Some(2))
    assert(board.valueAt(0,1) == None)
    assert(!board.isSolved)
    assert(!board.isUnsolvable)

    val newBoard = board.solve.get
    println(s"SOLUTION: \n$newBoard")
  }

  test("Intense test -> 6 Boards") {
    val a = parse("..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..")
    val b = parse("2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3")
    val c = parse(".3..5..4...8.1.5..46.....12.7.5.2.8....6.3....4.1.9.3.25.....98..1.2.6...8..6..2.")
    val d = parse(".2.81.74.7....31...9...28.5..9.4..874..2.8..316..3.2..3.27...6...56....8.76.51.9.")
    val e = parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.")
    val f = parse("48...69.2..2..8..19..37..6.84..1.2....37.41....1.6..49.2..85..77..9..6..6.92...18")
    val list = List(a,b,c,d,e,f)
    list.map(b => run(b))
    def run(board: Board) = {
      println(s"ORIGINAL BOARD: \n$board")
      assert(!board.isSolved)
      assert(!board.isUnsolvable)
      val newBoard = board.solve.get
      println(s"SOLUTION: \n$newBoard")

    }
  }

  test("Piazza Boards"){
    println("1 Done")
    assert(parse("85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58").solve.get.isSolved)
    println("2 Done")
    assert(parse(".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2").solve.get.isSolved)
    println("3 Done")
    assert(parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.").solve.get.isSolved)
    println("4 Done")
    assert(parse("2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3").solve.get.isSolved)
    println("5 Done")
    assert(parse("..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..").solve.get.isSolved)
    println("6 Done")
    assert(parse(".2.81.74.7....31...9...28.5..9.4..874..2.8..316..3.2..3.27...6...56....8.76.51.9.").solve.get.isSolved)
    println("7 Done")
    assert(parse(".3..5..4...8.1.5..46.....12.7.5.2.8....6.3....4.1.9.3.25.....98..1.2.6...8..6..2.").solve.get.isSolved)
    println("8 Done")
    assert(parse("2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3").solve.get.isSolved)
    println("9 Done")
    assert(parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.").solve.get.isSolved)
    println("10 Done")
    assert(parse("48...69.2..2..8..19..37..6.84..1.2....37.41....1.6..49.2..85..77..9..6..6.92...18").solve.get.isSolved)
    println("11 Done")
    assert(parse("...1254....84.....42.8......3.....95.6.9.2.1.51.....6......3.49.....72....1298...").solve.get.isSolved)
    println("12 Done")
    assert(parse(".6234.75.1....56..57.....4.....948..4.......6..583.....3.....91..64....7.59.8326.").solve.get.isSolved)
    println("13 Done")
    assert(parse("3..........5..9...2..5.4....2....7..16.....587.431.6.....89.1......67.8......5437").solve.get.isSolved)
    println("14 Done")
    //assert(parse("....2..4...8.35.......7.6.2.31.4697.2...........5.12.3.49...73........1.8....4...").solve.get.isSolved)
    println("15 -> Needs Fallback")
    assert(parse("361.259...8.96..1.4......57..8...471...6.3...259...8..74......5.2..18.6...547.329").solve.get.isSolved)
    println("16 Done")
    assert(parse(".5.8.7.2.6...1..9.7.254...6.7..2.3.15.4...9.81.3.8..7.9...762.5.6..9...3.8.1.3.4.").solve.get.isSolved)
    println("17 Done")
    assert(parse(".8...5........3457....7.8.9.6.4..9.3..7.1.5..4.8..7.2.9.1.2....8423........1...8.").solve.get.isSolved)
    println("18 Done")
    assert(parse("..35.29......4....1.6...3.59..251..8.7.4.8.3.8..763..13.8...1.4....2......51.48..").solve.get.isSolved)
    println("19 Done")
    assert(parse("...........98.51...519.742.29.4.1.65.........14.5.8.93.267.958...51.36...........").solve.get.isSolved)
    println("20 Done")
    assert(parse(".2..3..9....9.7...9..2.8..5..48.65..6.7...2.8..31.29..8..6.5..7...3.9....3..2..5.").solve.get.isSolved)
    println("21 Done")
    assert(parse("..5.....6.7...9.2....5..1.78.415.......8.3.......928.59.7..6....3.4...1.2.....6..").solve.get.isSolved)
    println("22 Done")
    //assert(parse(".4.....5...19436....9...3..6...5...21.3...5.68...2...7..5...2....24367...3.....4.").solve.get.isSolved)
    println("23  -> Needs Fallback")
    assert(parse("..4..........3...239.7...8.4....9..12.98.13.76..2....8.1...8.539...4..........8..").solve.get.isSolved)
    println("24 Done")
    assert(parse("..72564..4.......5.1..3..6....5.8.....8.6.2.....1.7....3..7..9.2.......4..63127..").solve.get.isSolved)
    println("25 Done")
    assert(parse("..........79.5.18.8.......7..73.68..45.7.8.96..35.27..7.......5.16.3.42..........").solve.get.isSolved)
    println("26 Done")
    assert(parse(".3.....8...9...5....75.92..7..1.5..8.2..9..3.9..4.2..1..42.71....2...8...7.....9.").solve.get.isSolved)
    println("27 Done")
    assert(parse("2..17.6.3.5....1.......6.79....4.7.....8.1.....9.5....31.4.......5....6.9.6.37..2").solve.get.isSolved)
    println("28 Done")
    //assert(parse(".......8.8..7.1.4..4..2..3.374...9......3......5...321.1..6..5..5.8.2..6.8.......").solve.get.isSolved)
    println("29  -> Needs Fallback")
    assert(parse("6.8.7.5.2.5.6.8.7...2...3..5...9...6.4.3.2.5.8...5...3..5...2...1.7.4.9.4.9.6.7.1").solve.get.isSolved)
    println("30 Done")
    assert(parse(".5..1..4.1.7...6.2...9.5...2.8.3.5.1.4..7..2.9.1.8.4.6...4.1...3.4...7.9.2..6..1.").solve.get.isSolved)
    println("31 Done")
    assert(parse(".53...79...97534..1.......2.9..8..1....9.7....8..3..7.5.......3..76412...61...94.").solve.get.isSolved)
    println("32 Done")
    assert(parse("..6.8.3...49.7.25....4.5...6..317..4..7...8..1..826..9...7.2....75.4.19...3.9.6..").solve.get.isSolved)
    println("33 Done")
    assert(parse("..5.8.7..7..2.4..532.....84.6.1.5.4...8...5...7.8.3.1.45.....916..5.8..7..3.1.6..").solve.get.isSolved)
    println("34 Done")
    //assert(parse("...9..8..128..64...7.8...6.8..43...75.......96...79..8.9...4.1...36..284..1..7...").solve.get.isSolved)
    println("35  -> Needs Fallback")
    assert(parse("....8....27.....54.95...81...98.64...2.4.3.6...69.51...17...62.46.....38....9....").solve.get.isSolved)
    println("36 Done")
    assert(parse("...6.2...4...5...1.85.1.62..382.671...........194.735..26.4.53.9...2...7...8.9...").solve.get.isSolved)
    println("37 Done")
    assert(parse("38..........4..785..9.2.3...6..9....8..3.2..9....4..7...1.7.5..495..6..........92").solve.get.isSolved)
    println("38 Done")
    assert(parse("...158.....2.6.8...3.....4..27.3.51...........46.8.79..5.....8...4.7.1.....325...").solve.get.isSolved)
    println("39 Done")
    //assert(parse(".8.....4....469...4.......7..59.46...7.6.8.3...85.21..9.......5...781....6.....1.").solve.get.isSolved)
    println("40  -> Needs Fallback")
  }
}
