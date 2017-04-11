package com.ot.flac.decoder

import java.nio.channels.Channels

import org.scalatest._

/**
  * Created by tstevens on 4/7/17.
  */
class DecodeTest extends FunSuite {

  import Decode._

  val arFF = Array[Byte](0xFF.toByte, 0x00)

  test("bit to int F0, 2, 2") {
    val x = bitToInt(arFF, 2, 2)
    assert ( x == 3)
  }

  test("bit to long F0,6,4") {
    val x = bitToInt(arFF, 6, 4)
    assert(x == 12)

//    x match {
//      case Right(l) => assert(l === 12)
//      case x => fail(s"expected right, received: ${x}")
//    }
  }

  test("bit to long F0,0,16") {
    val x = bitToInt(arFF, 0, 16)
    assert(x == 0xFF00)
//    x match {
//      case Right(l) => assert(l === 255)
//      case x => fail(s"expected right, received: ${x}")
//    }

  }

  val arFF995511 = Array[Byte](0xFF.toByte, 0x99.toByte, 0x55, 0x11)

  test("bit to long FF995511, 5, 22") {
    val x = bitToInt(arFF995511, 5, 22)
    assert(x === 0x3CCAA8)
  }

  test("bit to long ff995511, 0, 31") {
    val x = bitToInt(arFF995511, 0, 31)
    assert(x === 0x7FCCAA88)
  }

  test("bit to long FF995511, 5, 27") {
    val x = bitToInt(arFF995511, 5, 27)
    assert(x === 0x07995511)
  }


//  test("read a flac file") {
//    val flacStream = getClass().getResourceAsStream("/HeavenlyGate.flac")
//    val channel = Channels.newChannel(flacStream)
//
//    val result = Input.decode(channel)
//
//    result match {
//      case Left(x) => fail("expected right, received: " + x)
//      case Right(metadata) => {
//        assert(metadata.length === 1)
//        println(s"MD: ${metadata}")
//      }
//    }
//
//  }


}
