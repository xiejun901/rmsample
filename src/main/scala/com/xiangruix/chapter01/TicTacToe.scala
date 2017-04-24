package com.xiangruix.chapter01

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.util.Random

/**
  * Created by xiejun on 2017/2/19.
  */
object TicTacToe {

    def main(args: Array[String]): Unit = {
        println("tictactoe")
        compete()
    }

    def train(epochs: Int = 20000) = {
        val p1 = new AIPlayer(-1)
        val p2 = new AIPlayer(1)
        val judger = new Judger(p1, p2, true)

        @tailrec
        def loop(j: Judger, p1WonCnt: Int, p2WonCnt: Int, tieCnt: Int, leftCnt: Int): (Int, Int, Int) = {
            leftCnt match {
                case 0 => (p1WonCnt, p2WonCnt, tieCnt)
                case _ =>
                    val winner = judger.play(true)
                    val (cnt1, cnt2, cnt) = winner match {
                        case None => (p1WonCnt, p2WonCnt, tieCnt + 1)
                        case Some(winp) if winp == p1 => (p1WonCnt + 1, p2WonCnt, tieCnt)
                        case Some(winp) if winp == p2 => (p1WonCnt, p2WonCnt + 1, tieCnt)
                    }
                    loop(j, cnt1, cnt2, cnt, leftCnt - 1)
            }
        }

        val (p1WonCnt, p2WonCnt, tieCnt) = loop(judger, 0, 0, 0, epochs)
        println(s"p1 win $p1WonCnt times, p2 win $p2WonCnt times, tie $tieCnt times, total $epochs times!")
        println("p1 win: " + p1WonCnt.toDouble / epochs.toDouble)
        println("p2 win: " + p2WonCnt.toDouble / epochs.toDouble)
        p1.savePolicy("policy_-1")
        p2.savePolicy("policy_1")

    }

    def compete(turns:Int = 500) = {
        val p1 = new AIPlayer(-1, exploreRate = 0.0)
        val p2 = new AIPlayer(1, exploreRate = 0.0)
        p1.loadPolicy("policy_-1")
        p2.loadPolicy("policy_1")
        val judger = new Judger(p1, p2, false)

        @tailrec
        def loop(j: Judger, p1WonCnt: Int, p2WonCnt: Int, tieCnt: Int, leftCnt: Int): (Int, Int, Int) = {
            leftCnt match {
                case 0 => (p1WonCnt, p2WonCnt, tieCnt)
                case _ =>
                    val winner = judger.play(true)
                    val (cnt1, cnt2, cnt) = winner match {
                        case None => (p1WonCnt, p2WonCnt, tieCnt + 1)
                        case Some(winp) if winp == p1 => (p1WonCnt + 1, p2WonCnt, tieCnt)
                        case Some(winp) if winp == p2 => (p1WonCnt, p2WonCnt + 1, tieCnt)
                    }
                    loop(j, cnt1, cnt2, cnt, leftCnt - 1)
            }
        }

        val (p1WonCnt, p2WonCnt, tieCnt) = loop(judger, 0, 0, 0, turns)
        println(s"p1 win $p1WonCnt times, p2 win $p2WonCnt times, tie $tieCnt times, total $turns times!")
        println("p1 win: " + p1WonCnt.toDouble / turns.toDouble)
        println("p2 win: " + p2WonCnt.toDouble / turns.toDouble)
    }

}

class State(val data: Array[Array[Int]]) {

    def rowNum: Int = data.length

    def colNum: Int = data.head.length

    var wonSymbol: Int = _

    lazy val isEnd = determinEnd

    def this(rowNum: Int, colNum: Int) = this(Array.ofDim[Int](rowNum, colNum))

    def this() = this(3, 3)

    override def hashCode: Int = {
        val mul = math.pow(3, colNum).toInt
        data.map(_.map(_ + 1)
            .reduce(_ * 3 + _))
            .reduce(_ * mul + _)
    }

    def freePlace: List[(Int, Int)] = {
        data.zipWithIndex
            .flatMap {
                case (arr, rowIndex) =>
                    arr.zipWithIndex.map {
                        case (v, colIndex) =>
                            (v, (rowIndex, colIndex))
                    }
            }
            .filter(_._1 == 0)
            .toList
            .map(_._2)
    }

    def takeAction(row: Int, col: Int, symbol: Int): State = {
        val d = data.map(_.clone())
        d(row)(col) = symbol
        new State(d)
    }

    def determinEnd: (Boolean, Int) = {
        val rowSum = data.map(_.sum).toList
        val colSum = data.foldLeft(Array.ofDim[Int](rowNum))(
            _.zip(_).map {
                case (x, y) => x + y
            }
        ).toList
        val hyperSum = data.zipWithIndex.map {
            case (arr, index) =>
                (arr(index), arr(rowNum - 1 - index))
        }.reduce((x, y) => (x._1 + y._1, x._2 + y._2))
            .productIterator
            .toList
        val absSum = data.map(_.map(math.abs).sum).sum
        val allSum = rowSum ::: colSum ::: hyperSum
        (allSum.find(_ == rowNum), allSum.find(_ == -1 * rowNum)) match {
            case (Some(_), None) => (true, 1)
            case (None, Some(_)) => (true, -1)
            case _ if absSum == rowNum * colNum => (true, 0)
            case _ => (false, 0)
        }
    }

    override def toString: String = data.map {
        _.map {
            case 1 => "x"
            case -1 => "O"
            case 0 => "*"
        }.mkString("|")
    }
        .mkString("\n")

    def show: Unit = {
        val panel = data.map {
            _.map {
                case 1 => "x"
                case -1 => "O"
                case 0 => "*"
            }.mkString("|")
        }
            .mkString("\n")
        println("-------------------")
        println(panel)
        println("-------------------")
    }

}

object State {

    lazy val allStates: Map[Int, EndType] = getAllState

    type EndType = (Boolean, Int)

    private def getAllState: Map[Int, EndType] = {
        val states = new collection.mutable.HashMap[Int, EndType]()
        val currentState = new State()
        val currentSymbol = -1
        states(currentState.hashCode) = currentState.isEnd

        def loop(curState: State, curSymbol: Int, states: collection.mutable.HashMap[Int, EndType]): Unit = {
            curState.freePlace.foreach {
                case (rowIndex, colIndex) =>
                    val nextState = curState.takeAction(rowIndex, colIndex, curSymbol)
                    states(nextState.hashCode) = nextState.isEnd
                    if (!nextState.isEnd._1) {
                        loop(nextState, -1 * curSymbol, states)
                    }
            }
        }

        loop(currentState, currentSymbol, states)
        states.toMap
    }

}

abstract class Player(val symbol: Int) {
    def takeAction(): (Int, Int, Int)

    def reset: Unit

    def feedReward(reward: Double): Unit

    def feedState(state: State): Unit

    override def toString: String = s"symbol: $symbol"
}

class AIPlayer(symbol: Int, val stepSize: Double = 0.1, val exploreRate: Double = 0.1) extends Player(symbol) {

    var stateList: List[State] = Nil
    var estimations: collection.mutable.Map[Int, Double] = collection.mutable.Map(initEstimation.toSeq: _*)

    def reset: Unit = {
        stateList = Nil
    }

    def feedState(state: State): Unit = {
        stateList = state :: stateList
    }

    private def initEstimation = {
        State.allStates.map {
            case (hashCode, (isEnd, winnerSymbol)) =>
                val value = if (isEnd && winnerSymbol == symbol) 1.0
                else if (isEnd && winnerSymbol != symbol) -1.0
                else 0.5
                (hashCode, value)
        }
    }

    def feedReward(reward: Double): Unit = {

        @tailrec
        def loop(stateList: List[State], target: Double, estimations: collection.mutable.Map[Int, Double]): Unit = {
            stateList match {
                case Nil => Unit
                case currState :: xs =>
                    val hashCode = currState.hashCode
                    val value = estimations(hashCode) + stepSize * (target - estimations(hashCode))
                    estimations(hashCode) = value
                    loop(xs, value, estimations)
            }
        }

        loop(stateList, reward, estimations)
        reset
    }

    def takeAction: (Int, Int, Int) = {
        val state = stateList.head
        val next = state.freePlace.map {
            case (row, col) =>
                (row, col, estimations(state.takeAction(row, col, symbol).hashCode))
        }
        val (r, c, e) = if (Random.nextDouble() < exploreRate) {
            Random.shuffle(next).head
        } else {
            Random.shuffle(next).sortWith((x, y) => x._3 > y._3).head
        }
        (r, c, symbol)
    }

    def savePolicy(path: String): Unit = {
        val out = new ObjectOutputStream(new FileOutputStream(path))
        out.writeObject(estimations)
        out.close()
    }

    def loadPolicy(path: String): Unit = {
        val in = new ObjectInputStream(new FileInputStream(path))
        val e = in.readObject().asInstanceOf[collection.mutable.Map[Int, Double]]
        in.close()
        estimations = e
    }
}

class Judger(val p1: Player, val p2: Player, val isFeedBack: Boolean) {

    def play(isShow: Boolean = false) = {
        val winPlayer = Judger.playLoop(new State(), p1, p2, isFeedBack, isShow)
        winPlayer match {
            case None => println("平局")
            case Some(winP) if winP == p1 => println("p1 win")
            case Some(winP) if winP == p2 => println("p2 win")
        }
        winPlayer
    }

    def reset = {
        p1.reset
        p2.reset
    }
}

object Judger {

    @tailrec
    private def playLoop(
        currState: State,
        currPlayer: Player,
        nextPlayer: Player,
        isFeedBack: Boolean,
        isShow: Boolean
    ): Option[Player] = {
        if (isShow) {
            currState.show
        }
        currPlayer.feedState(currState)
        nextPlayer.feedState(currState)
        // 之所以不直接用state.isEnd是因为计算状态是否结束复杂度较高，所以提前计算下来存好
        State.allStates(currState.hashCode) match {
            case (true, winner) =>
                if (winner == currPlayer.symbol) {
                    if (isFeedBack) {
                        currPlayer.feedReward(1.0)
                        nextPlayer.feedReward(0.0)
                    }
                    Some(currPlayer)
                } else if (winner == nextPlayer.symbol) {
                    if (isFeedBack) {
                        currPlayer.feedReward(0.0)
                        nextPlayer.feedReward(1.0)
                    }
                    Some(nextPlayer)
                } else {
                    if (isFeedBack) {
                        currPlayer.feedReward(0.0)
                        nextPlayer.feedReward(0.0)
                    }
                    None
                }
            case (false, _) =>
                val (r, c, s) = currPlayer.takeAction()
                val nextState = currState.takeAction(r, c, s)
                playLoop(nextState, nextPlayer, currPlayer, isFeedBack, isShow)
        }
    }
}

class HumanPlayer(symbol: Int) extends Player(symbol) {

    var currentState: State = null

    override def feedReward(reward: Double) = {
    }

    override def feedState(state: State) = {
        currentState = state
    }

    override def reset = {

    }

    override def takeAction(): (Int, Int, Int) = {
        println("please in put the position: ")
        val line = scala.io.StdIn.readLine()
        val Array(row, col) = line.split(" ").map(_.toInt)
        (row, col, symbol)
    }

}
