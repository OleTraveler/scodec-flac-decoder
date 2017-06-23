package com.ot.flac.decoder

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.Charset

import scodec.bits.{BitVector, ByteOrdering}


/**
  * Created by tstevens on 4/6/17.
  */
object Decode {

  import com.ot.util.ByteUtils._

  object DecodeError {
    def apply(message: String): DecodeError = DecodeError(message, None)
    def apply(message: String, throwable: Throwable): DecodeError = DecodeError(message, Some(throwable))
  }
  case class DecodeError(message: String, exception: Option[Throwable])

  /** Convenience method to load some bytes from the channel */
  private def nextBytes(numBytes: Int, channel: ReadableByteChannel) : Either[DecodeError, Array[Byte]] = {
    val flacBuff = ByteBuffer.allocate(numBytes)
    channel.read(flacBuff)
    flacBuff.flip()
    val array = flacBuff.array()
    if (array.length == numBytes) Right(array)
    else Left(DecodeError(s"Tried to read ${numBytes} from channel, but only ${array.length} was read."))
  }

  private def nextBitVector(numBytes: Int, channel: ReadableByteChannel) : BitVector = {
    val flacBuff = ByteBuffer.allocate(numBytes)
    channel.read(flacBuff)
    flacBuff.flip()
    BitVector(flacBuff)
  }

  implicit class IntPower(i: Int) {
    def ** (j: Int): Int = (1 to j).fold(1)( (i1, i2) => i1 * i)
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

  private def readMetadataBlockHeader2(ch: ReadableByteChannel): Either[DecodeError, MetadataBlockHeader] = {
    import scodec.codecs._

    val comb = bool ~ byte(7) ~ uint(24)
    val bv = nextBitVector(4, ch)

    for {
      dec <- comb.decode(bv)
    } yield {
      dec.map(x => MetadataBlockHeader(x._1._1, x._1._2, x._2))
    }

  }

  private def readMetadataBlockHeader(ch: ReadableByteChannel): Either[DecodeError, MetadataBlockHeader] = {

    for {
      bytes <- nextBytes(4, ch)
      lastBlock = (bytes(0) & 0x8) == 1 //the first bit is a boolean.
      blockType <- blockTypeFromByte(bytes(0))

      length = {
        bytes.update(0,0) //0 out the 8th bit
        ByteBuffer.wrap(bytes).getInt
      }
    } yield MetadataBlockHeader(lastBlock, blockType, length)
  }

  def readMetadataType(header: MetadataBlockHeader, body: Array[Byte]) : Either[DecodeError, Metadata] = {
    header.blockType match {
      case MetadataBlockHeader.STREAMINFO => readStreamInfo(body)
      case MetadataBlockHeader.PADDING => Right(Padding(body.length))
      case MetadataBlockHeader.APPLICATION => readApplication(body)
      case MetadataBlockHeader.SEEKTABLE => readSeektable(body)
      case MetadataBlockHeader.VORBIS_COMMENT => readVorbisComment(body)
      case MetadataBlockHeader.CUESHEET => ???
      case MetadataBlockHeader.PICTURE => ???
      case x => Left(DecodeError(s"Invalid blockType: ${x}"))
    }
  }

  def nowForTheMetadata(ch: ReadableByteChannel) : Either[DecodeError, List[(MetadataBlockHeader, Metadata)]] = for {
    header <-readMetadataBlockHeader(ch)
    bodyBytes <- nextBytes(header.length, ch)
    mdBody <- readMetadataType(header, bodyBytes)
    result <- {
      val tuple = (header, mdBody)
      if (header.lastMetadataBlock) Right(List( tuple ))
      else nowForTheMetadata(ch).map( tuple :: _)
    }
  } yield result

  def readSeektable(outerBytes: Array[Byte]) : Either[DecodeError, SeekTable] = {

    //144 bytes per each seekpoint
    def readSeekPoint(bytes: Array[Byte], byteOffset: Int) : Either[DecodeError, List[SeekPoint]] = {
      if (byteOffset > bytes.length) Right(List.empty)
      else if (byteOffset - bytes.length % 18 != 0) Left(DecodeError(s"expected offset - bytes.length % 144 0= 0, but is ${byteOffset - bytes.length % 144}"))
      else {
        for {
          sample <- byteToLong(bytes, 0, 8).left.map(DecodeError("While reading sample", _))
          offset <- byteToLong(bytes, 8, 8).left.map(DecodeError("While reading offset", _))
          numSamples <- byteToInt(bytes, 16, 2).left.map(DecodeError("While reading numSamples", _))
          result <- {
            readSeekPoint(bytes, byteOffset + 18).map(SeekPoint(sample, offset, numSamples) :: _)
          }
        } yield result
      }
    }

    readSeekPoint(outerBytes, 0).map(SeekTable(_))

  }


  def readApplication(bytes: Array[Byte]) : Either[DecodeError, Application] = for {
      appId <- byteToInt(bytes, 0, 4).left.map(DecodeError("While reading application id", _))
  } yield Application(appId, bytes.drop(4))


  /** Read the StreamInfo [https://xiph.org/flac/format.html#metadata_block_streaminfo] block.
    * @param bytes Should contain bytes specific to StreamInfo.
    **/
  def readStreamInfo(bytes: Array[Byte]) : Either[DecodeError, StreamInfo] = {

    for {
      minBlockSize <- byteToInt(bytes, 0, 2).left.map(DecodeError("While reading minBlockSize", _))
      maxBlockSize <- byteToInt(bytes, 2, 2).left.map(DecodeError("While reading maxBlockSize", _))
      minFameSize <- byteToInt(bytes, 4, 3).left.map(DecodeError("While reading minFrameSize", _))
      maxFrameSize <- byteToInt(bytes, 7, 3).left.map(DecodeError("While reading maxFrameSize", _))
      sampleRate <- bitToInt(bytes, 80, 20).left.map(DecodeError("While reading sampleRate", _))
      numberOfChannels <- bitToInt(bytes, 100, 3).map(_ + 1).left.map(DecodeError("While reading numberOfChannels", _))
      bitsPerSample <- bitToInt(bytes, 103, 5).map(_ + 1).left.map(DecodeError("While reading bitsPerSample", _))
      totalStreams <- bitToLong(bytes, 108, 36).left.map(DecodeError("While reading totalStreams", _))
      md5 <- bitToBigInteger(bytes, 144, 128).left.map(DecodeError("While reading md5", _))

    } yield StreamInfo(minBlockSize, maxBlockSize, minFameSize, maxFrameSize, sampleRate, numberOfChannels, bitsPerSample, totalStreams, md5)
  }

  def readUnsignedIntBytes(bytes: Array[Byte], begin: Int, length: Int): (String, Array[Byte]) = {
    if (length > 0) {
      (new String(bytes, begin, length, "UTF-8"), bytes.drop(begin + length))
    } else {
      val s1 = new String(bytes, begin, Integer.MAX_VALUE)
      val b2 = bytes.drop(begin).drop(Integer.MAX_VALUE)
      (s1 + new String(b2, 0, length - Integer.MAX_VALUE), b2.drop(length - Integer.MAX_VALUE)
    }
  }

  def readVorbisComment(bytes: Array[Byte]): Either[DecodeError, VorbisComment] = {

    for {
      vendorLength <- byteToInt(bytes, 0, 4).left.map(DecodeError("While reading vendorLength", _))
      vendorStringRemainingBytes <- readUnsignedIntBytes(bytes, 4, vendorLength)
      comentListLength <-
    }

  }


  /** Read the Flac Metadata from the beginning of the file. */
  def decode(channel: ReadableByteChannel) : Either[DecodeError,List[Metadata]] = {

    //ensure this is the start of the flac channel
    for {
      marker <- nextBytes(4, channel)
      _ <- isCorrectMarker(marker)
      streamInfoHeader <- readMetadataBlockHeader(channel)
      streamInfoBytes <- nextBytes(streamInfoHeader.length, channel)
      streamInfo <- readStreamInfo(streamInfoBytes)
    } yield List(streamInfoHeader, streamInfo)


  }

}
