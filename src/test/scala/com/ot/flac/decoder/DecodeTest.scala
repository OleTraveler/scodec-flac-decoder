package com.ot.flac.decoder

import java.nio.channels.Channels

import org.scalatest.FunSuite

/**
  * Created by tstevens on 4/7/17.
  */
class DecodeTest extends FunSuite {


  test("read a flac file") {
    val flacStream = getClass().getResourceAsStream("/HeavenlyGate.flac")
    val channel = Channels.newChannel(flacStream)

    val result = Input.decode(channel)

    result match {
      case Left(x) => fail("expected right, received: " + x)
      case Right(metadata) => {
        assert(metadata.length === 1)
        println(s"MD: ${metadata}")
      }
    }

  }


}
