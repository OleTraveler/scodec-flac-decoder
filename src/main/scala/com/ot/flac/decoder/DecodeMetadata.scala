package com.ot.flac.decoder

import org.jflac.frame.Frame
import org.jflac.{FLACDecoder, FrameListener}


/**
  * Created by tstevens on 6/22/17.
  */
object DecodeMetadata extends App {

  import java.nio.file.{Files, Paths}

//  val path = Paths.get("/Users/tstevens/playspace/flaculence/flac-decoder/src/test/resources/blah.flac")
//  val path = Paths.get("/Users/tstevens/playspace/quodlibet/quodlibet/tests/data/sine-110hz.flac")
//  val path = Paths.get("/Users/tstevens/Music/flac/nervesandgel - Causality Loops/nervesandgel - Causality Loops - 01 Point Zero - Heavenly Gate.flac")
//  val path = Paths.get("/Users/tstevens/playspace/jc/mac/Jimmy Cliff - Reggae Greats/Jimmy Cliff - Reggae Greats - 01 - Vietnam.flac")
//  val path = Paths.get("/Users/tstevens/playspace/jc/lx/Jimmy Cliff/Jimmy Cliff-Reggae Greats/01 - Jimmy Cliff - Vietnam.flac")
  val path = Paths.get("/Users/tstevens/playspace/jc/mintmin/Jimmy Cliff/Reggae Greats/Disc 1 - 01 - Vietnam.flac")
  val is = Files.newInputStream(path)

  val decoder = new FLACDecoder(is)

  var count = 0

  val fl = new FrameListener {
    override def processFrame(frame: Frame): Unit = {
      val crc = frame.getCRC
      println(s"CRC count ${count} : '${crc & 0xffff}'  HEX: ${Integer.toHexString(crc & 0xffff)}")

      count = count + 1

    }

    override def processError(msg: String): Unit = sys.error("Error: " + msg)

    override def processMetadata(metadata: org.jflac.metadata.Metadata): Unit = {
      println("METADATA: " + metadata)
    }
  }
  decoder.addFrameListener(fl)

  decoder.decode()


//  val data = Files.readAllBytes(path)
//
//
//  val bv = BitVector(data)
//
//  val result = for {
//    metadata <- Metadata.decodeHeaders(bv)
//    f1 <- Frame.headerVariableOrCoded.decode(metadata.remainder)
//  } yield {
//    (metadata, f1)
//  }
//
//
//  result match {
//    case Successful(res) => {
//      println("Metadata:" + res._1)
//      println(s"Frame: ${res._2.value}")
//      println(s"remainder: ${res._2.remainder}")
//    }
//    case f: Failure => println(s"OOPs: ${f.cause}")
//  }






//  println(s"1: ${Metadata.hc.decode(bv)}")
//  println(s"2: ${Metadata.hc.decode(bv)}")
//  println(s"3: ${Metadata.hc.decode(bv)}")
//  println(s"4: ${Metadata.hc.decode(bv)}")
//  println(s"5: ${Metadata.hc.decode(bv)}")
//  println(s"6: ${Metadata.hc.decode(bv)}")
//  println(s"7: ${Metadata.hc.decode(bv)}")
//  println(s"8: ${Metadata.hc.decode(bv)}")
//  println(s"9: ${Metadata.hc.decode(bv)}")
//  println(s"10: ${Metadata.hc.decode(bv)}")
//  println(s"11: ${Metadata.hc.decode(bv)}")
//  println(s"12: ${Metadata.hc.decode(bv)}")

}
