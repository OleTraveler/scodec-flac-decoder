package com.ot.util

import java.security.MessageDigest

import org.scalatest.FunSuite
import sun.security.provider.MD5

/**
  * Created by tstevens on 4/11/17.
  */
class ByteUtilsTest extends FunSuite {

  import ByteUtils._

  val arFF = Array[Byte](0xFF.toByte, 0x00)
  val arFF995511 = Array[Byte](0xFF.toByte, 0x99.toByte, 0x55, 0x11)
  val arFFETC = Array[Byte](0xff.toByte, 0xee.toByte, 0xdd.toByte, 0xcc.toByte,
    0xbb.toByte, 0xaa.toByte, 0x99.toByte, 0x88.toByte, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0x00)

  test("bit to int F0, 2, 2") {
    val x = bitToInt(arFF, 2, 2)
    assert ( x.right.get == 3)

    assert( bitToLong(arFF, 2, 2) == x)
  }

  test("bit to long F0,6,4") {
    val x = bitToInt(arFF, 6, 4)
    assert(x.right.get == 12)

    assert( bitToLong(arFF, 6, 4) == x)
  }

  test("bit to long F0,0,16") {
    val x = bitToInt(arFF, 0, 16)
    assert(x.right.get == 0xFF00)
  }

  test("bit to long FF995511, 5, 22") {
    val x = bitToInt(arFF995511, 5, 22)
    assert(x.right.get === 0x3CCAA8)
  }

  test("bit to int FF995511, 18, 8") {
    val x = bitToInt(arFF995511, 18, 8)
    assert(x.right.get == 0x54)

    val y = bitToLong(arFF995511, 18, 8)
    assert(x.right.get == 0x54)
  }

  test("bit to long ff995511, 0, 31") {
    val x = bitToLong(arFF995511, 0, 31)
    assert(x.right.get === 0x7FCCAA88)

  }

  test("bit to long FF995511, 5, 27") {
    val x = bitToLong(arFF995511, 5, 27)
    assert(x.right.get === 0x07995511)
  }

  test("too many byte") {
    val x = bitToInt(arFFETC, 5, 33)
    assert( x.left.toOption.map(_.getMessage).exists(_ == "length argument max is 24, but is 33") )
  }

  test("read 3 to 32") {
    val x = bitToInt(arFFETC, 3, 23)
    x match {
      case Right(x) => assert(x === 0x7FBB77)
      case Left(x) => fail(s"Expected success, received: $x")
    }
  }

  test("read to far") {
    val x = bitToInt(arFF, 22, 22)
    x match {
      case Right(x) => fail("expected failure")
      case Left(ex) => assert(ex.getMessage == s"arguments will result in reading past array end.  buf length: ${arFF.length}, offset: 22, length: 22")
    }
  }


  test("arToLong: arFFetc, 0 8") {
    val x = bitToLong(arFFETC, 0, 8)
    x match {
      case Right(x) => assert(x == 0xFF)
      case Left(ex) => fail(s"expected success, received: ${ex}")
    }
  }

  test("arToLong: arFFetc, 0, 56") {
    val x = bitToLong(arFFETC, 0, 56)
    x match {
      case Right(x) => assert(x == Long2long(java.lang.Long.valueOf("FFEEDDCCBBAA99", 16)))
      case Left(ex) => fail(s"expected success, received: ${ex}")
    }
  }

  //BigInteger *************************
  test("bitToBigInteger F0, 2, 2") {
    val x = bitToBigInteger(arFF, 2, 2)
    assert ( x.right.get.intValue == 3)
  }

  test("bitToBigInteger F0,6,4") {
    val x = bitToBigInteger(arFF, 6, 4)
    assert(x.right.get.intValue == 12)
  }

  test("bitToBigInteger F0,0,16") {
    val x = bitToBigInteger(arFF, 0, 16)
    assert(x.right.get.intValue == 0xFF00)
  }


  test("bitToBigInteger FF995511, 5, 22") {
    val x = bitToBigInteger(arFF995511, 5, 22)
    assert(x.right.get.intValue === 0x3CCAA8)
  }

  test("bitToBigInteger ff995511, 0, 31") {
    val x = bitToBigInteger(arFF995511, 0, 31)
    assert(x.right.get.longValue === 0x7FCCAA88)

  }

  test("bitToBigInteger FF995511, 5, 27") {
    val x = bitToBigInteger(arFF995511, 5, 27)
    assert(x.right.get.longValue == 0x07995511)
  }

  test("bitToBigInteger read 3 to 32") {
    val x = bitToBigInteger(arFFETC, 3, 23)
    x match {
      case Right(x) => assert(x.intValue === 0x7FBB77)
      case Left(x) => fail(s"Expected success, received: $x")
    }
  }

  test("bitToBigInteger read to far") {
    val x = bitToBigInteger(arFF, 22, 22)
    x match {
      case Right(x) => fail("expected failure")
      case Left(ex) => assert(ex.getMessage == s"arguments will result in reading past array end.  buf length: ${arFF.length}, offset: 22, length: 22")
    }
  }


  test("bitToBigInteger: arFFetc, 0 8") {
    val x = bitToBigInteger(arFFETC, 0, 8)
    x match {
      case Right(x) => assert(x.longValue == 0xFF)
      case Left(ex) => fail(s"expected success, received: ${ex}")
    }
  }

  test("bitToBigInteger: arFFetc, 0, 56") {
    val x = bitToBigInteger(arFFETC, 0, 56)
    x match {
      case Right(x) => assert(x.longValue == Long2long(java.lang.Long.valueOf("FFEEDDCCBBAA99", 16)))
      case Left(ex) => fail(s"expected success, received: ${ex}")
    }
  }


  test("read md5") {
    val md5 = MessageDigest.getInstance("MD5").digest("Chicken Leg".getBytes)
    val md5Surrounded = Array[Byte](0x00) ++ md5 ++ Array[Byte](0x00)
    val x = bitToBigInteger(md5Surrounded, 8, 128)

    x match {
      case Right(res) => {
        assert(res.toByteArray.length == 16)
        assert(res.toByteArray.deep == md5.deep)
      }
      case Left(ex) => fail(s"expected success, received: ${ex}")
    }

  }
}
