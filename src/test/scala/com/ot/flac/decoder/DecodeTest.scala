package com.ot.flac.decoder

import java.nio.channels.Channels

import org.scalatest._

/**
  * Created by tstevens on 4/7/17.
  */
class DecodeTest extends FunSuite {

  import Decode._



  test("read a flac file") {
    val flacStream = getClass().getResourceAsStream("/blah.flac")
    val channel = Channels.newChannel(flacStream)

    val result = Decode.decode(channel)

    result match {
      case Left(x) => fail("expected right, received: " + x)
      case Right(metadata) => {
        assert(metadata.length === 2)

        println(s"MD: ${metadata}")
        val si = metadata.apply(1).asInstanceOf[StreamInfo]


        assert(si.minimumBlockSize == 4608)
        assert(si.maximumBlockSize == 4608)
        assert(si.minimumFrameSize == 1059)
        assert(si.maximumFrameSize == 4804)
        assert(si.sampleRate == 44100)
        assert(si.numberOfChannels == 1)
        assert(si.bitsPerSample == 16)
        assert(si.totalStreams == 158906)
        assert(si.md5.toString(16) == "20996b4bd00a20c6af9984b2ed035760")


      }
    }

  }


}
