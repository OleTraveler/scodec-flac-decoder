package com.ot.flac.decoder

import java.io.File

import scodec.bits.{BitVector, ByteVector}
import scodec.{Codec, Decoder}
import scodec.codecs._


/**
  * Created by tstevens on 6/22/17.
  */
object DecodeMetadata extends App {

  import java.nio.file.Files
  import java.nio.file.Paths

  val path = Paths.get("/Users/tstevens/playspace/flaculence/flac-decoder/src/test/resources/blah.flac")
  val data = Files.readAllBytes(path);
  val bv = BitVector(data)

  MetadataBlockHeader.marker
  val result = for {
    a0 <- MetadataBlockHeader.marker.decode(bv)
    a1 <- Metadata.hc.decode(a0.remainder)
    a2 <- Metadata.hc.decode(a1.remainder)
    a3 <- Metadata.hc.decode(a2.remainder)
  } yield {
    println(s"a1: ${a1.value}")
    println(s"a2: ${a2.value}")
    println(s"a3: ${a3.value}")

    Nil
  }

  println("Result:" + result)



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
