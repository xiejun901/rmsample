package com.xiangruix.chapter01

import org.junit._
import Assert._
/**
  * Created by xiejun on 2017/2/21.
  */
@Test
class TicTacToeTest {

}

@Test
class StateTest {

    @Test
    def testHashCode = {
        val state1 = new State(Array.tabulate(3,3)((_, _) => -1))
        assertEquals("状态hash值计算错误", 0, state1.hashCode)
        val state2 = new State(Array.tabulate(3,3)((_, _) => 1))
        assertEquals("状态hash值计算错误", (math.pow(3, 9) - 1).toInt, state2.hashCode)
    }

    @Test
    def testFreePlace = {
        val state1 = new State(
            Array(
                Array(1, 0, 1),
                Array(1, 0, 1),
                Array(1, 0, 1)
            )
        )
        assertEquals("还未放置的位置", List((0,1), (1,1), (2, 1)), state1.freePlace)

        val state2 = new State(
            Array(
                Array(1, 1, 1),
                Array(1, 1, 1),
                Array(1, 1, 1)
            )
        )
        assertEquals("还未放置的位置", List(), state2.freePlace)
    }

    @Test
    def testIsEnd = {
        val state1 = new State(
            Array(
                Array(1, 0, -1),
                Array(1, 0, -1),
                Array(1, 0, 0)
            )
        )
        assertEquals("游戏结束，1赢", (true, 1), state1.isEnd)
        val state2 = new State(
            Array(
                Array(-1, 0, -1),
                Array(1, -1, 1),
                Array(1, 0, -1)
            )
        )
        assertEquals("游戏结束，-1赢", (true, -1), state2.isEnd)
        val state3 = new State(
            Array(
                Array(-1, 1, -1),
                Array(1, -1, 1),
                Array(1, -1, 1)
            )
        )
        assertEquals("游戏结束，平局", (true, 0), state3.isEnd)

        val state4 = new State(
            Array(
                Array(0, 1, -1),
                Array(1, -1, 1),
                Array(1, -1, 1)
            )
        )
        assertEquals("游戏未结束", (false, 0), state4.isEnd)
    }

    @Test
    def testTakeAction={
        val state4 = new State(
            Array(
                Array(0, 1, -1),
                Array(1, -1, 1),
                Array(1, -1, 1)
            )
        )
        state4.show
        state4.takeAction(0,0,-1).show
        state4.show
    }

    @Test
    def testAllState = {
        val allState = State.allStates
        assertEquals("全部状态", 19683, allState.size)
    }
}

@Test
class AIPlayerTest{

    @Test
    def testFeedState = {
        val state1 = new State(
            Array(
                Array(0, 0, 0),
                Array(0, 0, 0),
                Array(0, 0, 0)
            )
        )
        val state2 = new State(
            Array(
                Array(0, 1, 0),
                Array(0, 0, 0),
                Array(0, 0, 0)
            )
        )
        val state3 = new State(
            Array(
                Array(0, 1, 0),
                Array(0, -1, 0),
                Array(0, 0, 0)
            )
        )
        val p = new AIPlayer(-1)
        p.feedState(state1)
        p.feedState(state2)
        p.feedState(state3)
        p.stateList.foreach(_.show)
    }
}

@Test
class JudgerTest{

    @Test
    def testHumanPlay() = {
        val p1 = new AIPlayer(-1)
        val p2 = new AIPlayer(1)
        val judger = new Judger(p1, p2, false)
        judger.play()
    }
}