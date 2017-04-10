package com.ot.flac.decoder

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.{Channel, ReadableByteChannel}


/**
  * Created by tstevens on 4/6/17.
  */
object Decode {

  object DecodeError {
    def apply(message: String): DecodeError = DecodeError(message, None)
  }
  case class DecodeError(message: String, exception: Option[Exception])

  /** Convenience method to load some bytes from teh channel */
  private def nextBytes(numBytes: Int, channel: ReadableByteChannel) : Either[DecodeError, Array[Byte]] = {
    val flacBuff = ByteBuffer.allocate(numBytes)
    channel.read(flacBuff)
    flacBuff.flip()
    val array = flacBuff.array()
    if (array.length == numBytes) Right(array)
    else Left(DecodeError(s"Tried to read ${numBytes} from channel, but only ${array.length} was read."))
  }

  implicit class IntPower(i: Int) {
    def ** (j: Int): Int = (1 to j).fold(1)( (i1, i2) => i1 * i)
  }

  def readInt(bytes: Array[Byte], bitStart: Int, readBits: Int): Either[String, Int] = {

    if (readBits > 8) {
      val startByte = bitStart / 0xF
      val startBitOffset = bitStart % 0xF

      val mask = 0xFF >> startBitOffset

      val left = bytes(startBitOffset) & mask

      val readExtra = readBits / 8
      val lastRead = readBits % 8
    }



    if (lastRead == 0)

//    val result = if (bits == 16) {
//      bytes(0) * (2 ** 8) + bytes(1)
//    } else {
//      -1
//    }

    val result = (iterate - 1 to 0).fold(0)( (l,n) => l + bytes(n) * (2 ** (8 * n)))

    Right(result)

  }

  /** aka. "fLaC" */
  val flacMarker = Array[Byte](0x66, 0x4C, 0x61, 0x43)

  /** ensure marker equals #flacMarker */
  private def isCorrectMarker(array: Array[Byte]) : Either[DecodeError, Array[Byte]] = {
    if (array.deep == flacMarker.deep) Right(array)
    else Left(DecodeError(s"Invalid flac marker, expected 'fLaC' (${new String(flacMarker, "UTF-8")}), received: (${new String(array, "UTF-8")})"))
  }

  private def blockTypeFromByte(byte: Byte) : Either[DecodeError,Byte] = {
    if (byte < 7) Right(byte)
    else Left(DecodeError(s"Invalid Block type: ${byte}"))
  }

  private def readMetadataBlockHeader(ch: ReadableByteChannel): Either[DecodeError, MetadataBockHeader] = {

    for {
      bytes <- nextBytes(8, ch)
      lastBlock = (bytes(0) & 0x8) == 1 //the first bit is a boolean.
      blockType <- blockTypeFromByte(bytes(0))

      length = {
        bytes.update(0,0) //0 out the 8th bit
        ByteBuffer.wrap(bytes).getInt
      }
    } yield MetadataBockHeader(lastBlock, blockType, length)
  }

  def readStreamInfo(body: Array[Byte]): Either[DecodeError, StreamInfo] = for {

  }

  def readMetadataType(header: MetadataBockHeader, body: Array[Byte]) : Either[DecodeError, Metadata] = {
    header.blockType match {
      case MetadataBockHeader.STREAMINFO => readStreamInfo(body)
      case _ => ???
    }
  }

  def nowForTheMetadata(ch: ReadableByteChannel) : Either[DecodeError, List[Metadata]] = for {
    header <-readMetadataBlockHeader(ch)
    bodyBytes <- nextBytes(header.length, ch)
    mdBody <- readMetadataType(header, bodyBytes)
  } yield ???

  def readStreamInfo(bytes: Array[Byte]) : Either[DecodeError, StreamInfo] = for {

  }



  def decode(channel: ReadableByteChannel) : Either[DecodeError,List[Metadata]] = {

    //ensure this is the start of the flac channel
    for {
      marker <- nextBytes(4, channel)
      _ <- isCorrectMarker(marker)
      streamInfoHeader <- readMetadataBlockHeader(channel)
      streamInfoBytes <- nextBytes(streamInfoHeader.length, channel)
      streamInfo <- readStreamInfo(streamInfoBytes)
      mdList <- nowForTheMetadata(channel)
    } yield mdList


  }

}
