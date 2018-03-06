package com.ot.flac.decoder

import scodec.codecs._
import scodec.bits._

import scodec.{Codec, Decoder}


/**
  * Created by tstevens on 6/23/17.
  */
object Frame {

  trait Frame

  val syncCode: Codec[Unit] = constant(bin"11111111111110")

  case class Header(isVariableBlockSize: Boolean, blockingSize: BitVector, sampleRate: BitVector, channelAssignment: BitVector, sampleSize: BitVector) extends Frame
  case class VariableBlockSize(codedSampleNumber: ByteVector) extends Frame
  case class CodedBlockSize(codedFrameNumber: ByteVector) extends Frame

  object Header {


//    /** Either coded sample number or coded frame number */
//    def sample(strategy: Codec[Boolean]): Codec[Either[CodedBlockSize, VariableBlockSize]] = either(strategy, , fixedSizeBits(48, utf8).as[VariableBlockSize])

    val baseCodec = syncCode :: bool :: bits(4) :: bits(4) :: bits(4) :: bits(3) :: constant(bin"0")
    val headerCodec: Codec[Header] = baseCodec.as[Header]



  }

  val headerVariableOrCoded: Decoder[(Header, Either[CodedBlockSize,VariableBlockSize])] =
    Header.headerCodec.flatMap(fh => {
      if (fh.isVariableBlockSize) bytes(48).as[VariableBlockSize].map(vbs => (fh, Right(vbs)))
      else bytes(40).as[CodedBlockSize].map(cbs => (fh, Left(cbs)))
    })




}
