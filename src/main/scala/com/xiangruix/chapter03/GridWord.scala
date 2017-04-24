package com.xiangruix.chapter03

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by xiejun on 2017/3/5.
  */
object GridWord {

    def main(args: Array[String]): Unit = {
        println("--------------Random policy evaluation-----------------------")
        val stateValue1 = Evaluator.evaluate(100,
            new RandomPolicy {
                override def isConverges(stateValue: StateValue, nextStateValue: StateValue): Boolean = false
            },
            new StateValue(5, 5))
        println(stateValue1.show)
        println("")

        println("--------------Optimal Policy by Value Iteration--------------")
        val stateValue2 = Evaluator.evaluate(2000,
            new ValueIterationPolicy {
                override def isConverges(stateValue: StateValue, nextStateValue: StateValue): Boolean = false
            },
            new StateValue(5, 5))
        println(stateValue2.show)
        println("")

        println("--------------Optimal Policy by Policy Iteration--------------")
        def loop(n: Int, p: PolicyIteration, sv: StateValue): StateValue = {
            n match {
                case 0 => sv
                case _ =>
                    val s = Evaluator.evaluate(1, p, sv)
                    val newP = p.greedy(s)
                    loop(n - 1, newP, s)
            }
        }

        val p = new PolicyIteration(List.tabulate(5, 5)((_, _) => (0.25, 0.25, 0.25, 0.25)))
        val s = new StateValue(5, 5)
        val stateValue3 = loop(1000, p, s)
        println(stateValue3.show)
        println("")

    }

}

trait Policy {
    def calculateValue(row: Int, col: Int, values: List[Double]): Double

    def isConverges(stateValue: StateValue, nextStateValue: StateValue): Boolean = false


}

class RandomPolicy extends Policy {
    val prob = List(0.25, 0.25, 0.25, 0.25)

    override def calculateValue(row: Int, col: Int, values: List[Double]): Double = values.zip(prob).map(x => x._1 * x._2).sum

    override def isConverges(stateValue: StateValue, nextStateValue: StateValue): Boolean =
        stateValue.value.zip(nextStateValue.value)
            .flatMap(x => x._1 zip x._2)
            .map(x => math.abs(x._1 - x._2))
            .forall(_ < 1e-4)
}

class ValueIterationPolicy extends Policy {
    override def calculateValue(row: Int, col: Int, values: List[Double]): Double = values.max

    override def isConverges(stateValue: StateValue, nextStateValue: StateValue): Boolean =
        stateValue.value.zip(nextStateValue.value)
            .flatMap(x => x._1 zip x._2)
            .map(x => math.abs(x._1 - x._2))
            .forall(_ < 1e-4)
}

class PolicyIteration(policy: List[List[(Double, Double, Double, Double)]]) extends Policy {

    override def calculateValue(row: Int, col: Int, values: List[Double]) =
        values.zip(policy(row)(col).productIterator.toList.map(_.asInstanceOf[Double])).map(x => x._1 * x._2).sum

    override def isConverges(stateValue: StateValue, nextStateValue: StateValue): Boolean = false

    def greedy(sv: StateValue) = {
        val p = List.tabulate(sv.value.length, sv.value.head.length)(
            (row, col) => {
                Random.shuffle(List("up", "down", "left", "right")
                    .map(State.takeAction(row, col, _))
                    .map {
                        case ((r, c), _) =>
                            sv.stateValue(r, c)
                    }.zipWithIndex)
                    .maxBy(_._1)
                    ._2 match {
                    case 0 => (1.0, 0.0, 0.0, 0.0)
                    case 1 => (0.0, 1.0, 0.0, 0.0)
                    case 2 => (0.0, 0.0, 1.0, 0.0)
                    case 3 => (0.0, 0.0, 0.0, 1.0)
                }
            }
        )
        new PolicyIteration(p)
    }


}


object Evaluator {

    val discount = 0.9

    def evaluate[T <: Policy](round: Int, policy: Policy, currStateValue: StateValue): StateValue = {
        @tailrec
        def loop(n: Int, stateValue: StateValue): StateValue = {
            n match {
                case 0 => stateValue
                case _ =>
                    val buf = for {
                        row <- stateValue.value.indices
                        col <- stateValue.value.head.indices
                    } yield {
                        val target = List("up", "down", "left", "right")
                            .map {
                                action =>
                                    val ((r, c), reward) = State.takeAction(row, col, action)
                                    stateValue.stateValue(r, c) * discount + reward
                            }
                        policy.calculateValue(row, col, target)
                    }
                    val rowLength = stateValue.value.length
                    val colLength = stateValue.value.head.length
                    val nextStateValue = new StateValue(
                        List.tabulate[Double](rowLength, colLength)((i, j) => buf(i * colLength + j))
                    )
                    policy.isConverges(stateValue, nextStateValue) match {
                        case true => nextStateValue
                        case false => loop(n - 1, nextStateValue)
                    }
            }
        }

        loop(round, currStateValue)
    }
}


class StateValue(val value: List[List[Double]]) {

    def this(colNum: Int, rowNum: Int) = this(List.tabulate(rowNum, colNum)((_, _) => 0))

    def stateValue(row: Int, col: Int) = value(row)(col)

    def show = value.map(_.map("%15.8f".format(_)).mkString(" ")).mkString("\n")
}

object State {

    val RowNum = 5
    val ColNum = 5
    val LastRowIndex = RowNum - 1
    val LastColIndex = ColNum - 1
    val APoisition = (0, 1)
    val APrimePosition = (4, 1)
    val Bposition = (0, 3)
    val BPrimePosition = (2, 3)

    def takeAction(row: Int, col: Int, action: String) = {
        (row, col) match {
            case State.APoisition =>
                ((State.APrimePosition._1, State.APrimePosition._2), 10.0)
            case State.Bposition =>
                ((State.BPrimePosition._1, State.BPrimePosition._2), 5.0)
            case _ =>
                action match {
                    case "up" =>
                        row match {
                            case 0 => ((row, col), -1.0)
                            case _ => ((row - 1, col), 0.0)
                        }
                    case "down" =>
                        row match {
                            case State.LastRowIndex => ((row, col), -1.0)
                            case _ => ((row + 1, col), 0.0)
                        }
                    case "left" =>
                        col match {
                            case 0 => ((row, col), -1.0)
                            case _ => ((row, col - 1), 0.0)
                        }
                    case "right" =>
                        col match {
                            case State.LastColIndex => ((row, col), -1.0)
                            case _ => ((row, col + 1), 0.0)
                        }
                }
        }
    }

}

