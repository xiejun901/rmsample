package com.xiangruix.chapter04

import scala.annotation.tailrec
import breeze.linalg._
import breeze.plot._

/**
  * Created by xiejun on 2017/4/13.
  */
object RandomWalk {

    val AllState = List(0, 1, 2, 3, 4, 5, 6)
    val InitialStateValue = List(0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0)

    type PredictionFun = (List[Double], Long, Double, Double) => List[Double]

    def main(args: Array[String]): Unit = {
//        val v1 = dpPrediction(InitialStateValue, 1000)
//        val v2 = mcPrediction(InitialStateValue, 1000000L, 0.001, 1.0)
//        val v3 = tdPrediction(InitialStateValue, 1000000L, 0.001, 1.0)
//        println("v1=" + v1)
//        println("v2=" + v2)
//        println("v3=" + v3)
        val error1 = RMSError(mcPrediction, 100, 201, 0.01)
//        val error2 = RMSError(mcPrediction, 100, 201, 0.04)
        val error3 = RMSError(tdPrediction, 100, 201, 0.1)
        val f = Figure()
        val p = f.subplot(0)
        val x =linspace(1, 201, 201)
        val y1 = DenseVector(error1.toArray)
//        val y2 = DenseVector(error2.toArray)
        val y3 = DenseVector(error3.toArray)
        p += plot(x, y1)
//        p += plot(x, y2, '.')
        p += plot(x, y3, '.', "y")
        p.xlabel = "episode"
        p.ylabel = "RMSError"
        f.saveas("rmse.png")
    }

    def RMSError(prediction: PredictionFun, runs:Long, episodes: Long, alpha:Double) = {
        val trueValue = dpPrediction(InitialStateValue, 1000)
        def loop(currValue: List[Double], error: List[Double], round: Long): List[Double] = {
            round match {
                case 0 => error
                case _ =>
                    val value = prediction(currValue, 1, alpha, 1.0)
                    val err = math.sqrt(trueValue.zip(value).map{
                        case (x1, x2) => (x1 - x2) * (x1 - x2)
                    }.sum / 5.0)
                    loop(value, err :: error, round - 1)
            }
        }
        def loop2(errsum: List[Double], round: Long):List[Double] = {
            round match {
                case 0 => errsum
                case _ =>
                    val error = loop(InitialStateValue, Nil, episodes)
                    val err = errsum.zip(error).map(x => x._1 + x._2)
                    loop2(err, round - 1)
            }
        }
        val errsum = loop2(List.tabulate[Double](episodes.toInt)(_ => 0.0), runs)
        errsum.map(_ / runs.toDouble).reverse
    }

    @tailrec
    def dpPrediction(stateValue: List[Double], round: Long): List[Double] = {
        round match {
            case 0 => stateValue
            case _ =>
                val newStateValue = AllState.map {
                    case 0 => 0
                    case 6 => 0
                    case x_ =>
                        val (nextLeft, rewardLeft) = x_.takeAction("left")
                        val (nextRith, rewardRight) = x_.takeAction("right")
                        0.5 * (stateValue(nextLeft) + rewardLeft) +
                            0.5 * (stateValue(nextRith) + rewardRight)
                }
                dpPrediction(newStateValue, round - 1)
        }
    }

    @tailrec
    def mcPrediction(currValue: List[Double], round: Long, alpha: Double, gama: Double): List[Double] = {
        round match {
            case 0 => currValue
            case _ =>
                @tailrec
                def update(g: Double, xs: List[(Int, Double)], value: Array[Double]): Array[Double] = {
                    xs match {
                        case Nil => value
                        case (state, reward) :: ys =>
                            val ng = gama * g + reward
                            value(state) = value(state) + alpha * (ng - value(state))
                            update(ng, ys, value)
                    }
                }
                val episode = genEpisode(Nil, 3)
                val newValue = update(0.0, episode, currValue.toArray)
//                println("round: " + round)
//                println("episode: " + episode)
//                println("state value: " + newValue.toList)
                mcPrediction(newValue.toList, round - 1L, alpha, gama)
        }
    }

    @tailrec
    def tdPrediction(currValue: List[Double], round: Long, alpha: Double, gama: Double): List[Double] = {
        round match {
            case 0 => currValue
            case _ =>
                @tailrec
                def update(xs: List[(Int, Double)], value: Array[Double]): Array[Double] = {
                    xs match {
                        case Nil => value
                        case List((state, reward)) =>
                            value(state) = value(state) + alpha * (reward - value(state))
                            value
                        case (state, reward) :: ys =>
                            val nextState = ys.head._1
                            val target = gama * value(nextState) + 0.0
                            value(state) = value(state) + alpha * (target - value(state))
                            update(ys, value)
                    }
                }
                val episode = genEpisode(Nil, 3).reverse
                val newValue = update(episode, currValue.toArray)
//                println("round: " + round)
//                println("episode: " + episode)
//                println("state value: " + newValue.toList)
                tdPrediction(newValue.toList, round - 1L, alpha, gama)
        }
    }



    @tailrec
    def genEpisode(epd: List[(Int, Double)], curState: Int): List[(Int, Double)] = {
        curState.isEnd match {
            case true => epd
            case false =>
                val action = if (util.Random.nextBoolean()) "left" else "right"
                val (next, reward) = curState.takeAction(action)
                genEpisode((curState, reward) :: epd, next)
        }
    }

    /**
      * 0 - 1 - 2 - 3 - 4 - 5 - 6
      *
      * @param x
      */
    implicit class State(
        x: Int
    ) {

        val Start = 0
        val End = 6

        def takeAction(direction: String) = {
            val res = direction match {
                case "left" =>
                    x - 1
                case "right" =>
                    x + 1
            }
            val reward = if (res == End) 1.0 else 0.0
            (res, reward)
        }

        def isEnd = x == Start || x == End

        override def hashCode(): Int = x
    }

}
