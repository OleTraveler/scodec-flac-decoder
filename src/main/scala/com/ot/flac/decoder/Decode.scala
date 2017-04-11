package com.ot.flac.decoder

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.{Channel, ReadableByteChannel}

import scala.annotation.tailrec
import scala.util.Try


/**
  * Created by tstevens on 4/6/17.
  */
object Decode {

  object DecodeError {
    def apply(message: String): DecodeError = DecodeError(message, None)
    def apply(message: String, throwable: Throwable): DecodeError = DecodeError(message, Some(throwable))
  }
  case class DecodeError(message: String, exception: Option[Throwable])

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

  /** Copied from https://gist.github.com/fbettag/3936752 */
  def byteToLong(buf: Array[Byte], offset: Int, length: Int): Either[DecodeError, Long] = Try {
    var ret = 0L
    var i = offset
    while (i < offset + length) {
      ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff)
      i += 1
    }
    ret
  }.toEither.left.map(t => DecodeError("exception while extracting toInt", t))

  def byteToInt(buf: Array[Byte], offset: Int, length: Int): Either[DecodeError, Int] = Try {
    var ret = 0
    var i = offset
    while (i < offset + length) {
      ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff)
      i += 1
    }
    ret
  }.toEither.left.map(t => DecodeError("exception while extracting toInt", t))


  def offsetToMask(offset: Int) : Byte = {
    offset match {
      case 0 => 0xFF.toByte
    }
  }

  private val BYTE_SIZE = 8

  /** Specify a bit offset (0 based) in an array of bytes and a length and the result will be
    * an integer representation of the bits popped off the array.
    *
    * For instance Array(0x77) is 0111 0111 and if we say offset 2, length 3 the result will be 101
    * @param buf The Byte array buffer.
    * @param offset The first bit to start with, 0 based index.
    * @param length The length
    * @return
    * @throws ArrayIndexOutOfBoundsException if offset + length > buf.length * 8
    */
  def bitToInt(buf: Array[Byte], offset: Int, length: Int): Int = {

    //the first index to read of buf
    val firstIndex = offset / BYTE_SIZE

    //the first bit to read of buf(firstIndex)
    val firstBit = offset % BYTE_SIZE

    //value is negative if the last bit is in the same byte as the first bit
    val lengthAfterFirstIndex = length - (BYTE_SIZE - firstBit)
    val fullIndexes = lengthAfterFirstIndex / BYTE_SIZE

    //any remaining bits to read after the full indexes have been read.
    //note: could also be negative
    val lastBit = lengthAfterFirstIndex % BYTE_SIZE

    //mask, so only the bits we care about are 1's.
    //start with the byte part all 1's and shift right to create a mask of just the bits we care about.
    //so for instance if firstBits is 6, the byte part should be 00000011
    val firstMask =  0x000000ff >> firstBit

    var i = 0
    val baseMask = 0x0000FFFF
    var ret = 0
    while (i <= fullIndexes) {

      val mask = if (i == 0 && lastBit >= 0) firstMask
      else if (i == 0 && lastBit < 0) {
        firstMask & (0xffffffff << Math.abs(lastBit))
      }
      else baseMask

      ret = ((ret << 8) & 0xffffffff) + (buf(i) & 0xff & mask)

      if (i == 0 && lastBit < 0) {
        ret = ret >> Math.abs(lastBit)
      }

      i += 1
    }

    if (lastBit > 0) {
      val mask = 0xffffff00 >> lastBit
      ret = ((ret << lastBit) & 0xffffffff) + ((buf(i) & mask & 0x000000ff) >> (BYTE_SIZE - lastBit))
      i = i + 1
    }

    ret
  } //.toEither.left.map(t => DecodeError("exception while extracting bitToLong", t))


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
    minBlockSize <- byteToInt(bytes, 0, 2)
    maxBlockSize <- byteToInt(bytes, 2, 2)
    minFameSize <- byteToInt(bytes, 4, 3)
    maxFrameSize <- byteToInt(bytes, 7, 3)

  } yield ???



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
