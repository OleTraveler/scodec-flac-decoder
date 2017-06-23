package com.ot.flac.decoder

import scodec.{Codec, Decoder}
import scodec.codecs._
import scodec.bits._


/**
  * Marker interface so we can extract all Metadata info.
  */
sealed trait Metadata {}


object Metadata {

  case class Header(isLastBlock: Boolean)
  case class Block(someData: Int)

  object Codec {
    val headerCodec : Codec[Header] = bool.as[Header]
    val blockCodec: Codec[Block] = int32.as[Block]
    val headerBlock: Codec[(Header, Block, Vector[(Header, Block)])] = ???

  }

  import MetadataBlockHeader._

  def blockHeaderToMetadataSection(header: MetadataBlockHeader) : Decoder[(MetadataBlockHeader, Metadata)] = {
    println("header.blockType:" + header.blockType)
    val codec = header.blockType match {
      case STREAMINFO => StreamInfo.codec
      case PADDING => Padding.codec(header.length)
      case APPLICATION => Application.codec(header.length)
      case SEEKTABLE => SeekTable.codec(header.length)
      case VORBIS_COMMENT => VorbisComment.codec
      case CUESHEET => CueSheet.codec
      case PICTURE => Picture.codec
      case x => sys.error(s"Invalid: $x")
    }
    codec.map(c => (header,c))
  }

  def hc: Decoder[(MetadataBlockHeader, Metadata)] = MetadataBlockHeader.codec.flatMap(blockHeaderToMetadataSection)



}

object MetadataBlockHeader {

  val marker: Codec[Unit] = constant(ByteVector("fLaC".getBytes))

  val STREAMINFO: Byte = 0
  val PADDING: Byte = 1
  val APPLICATION: Byte = 2
  val SEEKTABLE: Byte = 3
  val VORBIS_COMMENT: Byte = 4
  val CUESHEET: Byte = 5
  val PICTURE: Byte = 6
  //7-126 : reserved
  val INVALID: Byte = 127  //invalid, to avoid confusion with a frame sync code


  val baseCodec = bool :: byte(7) :: uint(24)
  val codec: Codec[MetadataBlockHeader] = baseCodec.as[MetadataBlockHeader]

}

/**
  *
  * @param lastMetadataBlock Last-metadata-block flag: '1' if this block is the last metadata block before the audio blocks, '0' otherwise.
  * @param blockType BLOCK_TYPE
  * @param length Length (in bytes) of metadata to follow (does not include the size of the METADATA_BLOCK_HEADER)
  */
case class MetadataBlockHeader(
  lastMetadataBlock: Boolean,
  blockType: Byte,
  length: Int
) extends Metadata

/**
  * This block has information about the whole stream, like sample rate, number of channels, total number of samples,
  * etc. It must be present as the first metadata block in the stream. Other metadata blocks may follow, and ones
  * that the decoder doesn't understand, it will skip.
  *
  * NOTES: FLAC specifies a minimum block size of 16 and a maximum block size of 65535, meaning the bit patterns
  * corresponding to the numbers 0-15 in the minimum blocksize and maximum blocksize fields are invalid.
  *
  * @param minimumBlockSize 16 bytes - The minimum block size (in samples) used in the stream.
  * @param maximumBlockSize 16 bytes - The maximum block size (in samples) used in the stream. (Minimum blocksize == maximum blocksize) implies a fixed-blocksize stream.
  * @param minimumFrameSize 24 bytes - The minimum frame size (in bytes) used in the stream. May be 0 to imply the value is not known.
  * @param maximumFrameSize 24 bytes - The maximum frame size (in bytes) used in the stream. May be 0 to imply the value is not known.
  * @param sampleRate 20 bytes - Sample rate in Hz. Though 20 bits are available, the maximum sample rate is limited by the structure of frame headers to 655350Hz. Also, a value of 0 is invalid.
  * @param numberOfChannels 3 bytes - "(number of channels)-1" - FLAC supports from 1 to 8 channels.  The 1
  * @param bitsPerSample 5 bytes - "(bits per sample)-1" FLAC supports from 4 to 32 bits per sample. Currently the reference encoder and decoders only support up to 24 bits per sample.
  * @param totalStreams 36 bytes - Total samples in stream. 'Samples' means inter-channel sample, i.e. one second of 44.1Khz audio will have 44100 samples regardless of the number of channels. A value of zero here means the number of total samples is unknown.
  * @param md5 128 bytes - MD5 signature of the unencoded audio data. This allows the decoder to determine if an error exists in the audio data even when the error does not result in an invalid bitstream.

  */
case class StreamInfo(
  minimumBlockSize: Int,
  maximumBlockSize: Int,
  minimumFrameSize: Int,
  maximumFrameSize: Int,
  sampleRate: Int,
  numberOfChannels: Int,
  bitsPerSample: Int,
  totalStreams: Long,
  md5: Vector[Byte]
) extends Metadata

object StreamInfo {
  val baseCodec = uint16 :: uint16 :: uint(24) :: uint(24) :: uint(20) :: uint(3) :: uint(5) :: ulong(36) :: vectorOfN(provide(16), byte)
  val codec: Codec[StreamInfo] = baseCodec.as[StreamInfo]
}


/**
  * n '0' bits (n must be a multiple of 8)
  * @param n number of Byte: 0's.
  */
case class Padding(n: Int) extends Metadata

object Padding {

  def codec(length: Int): Codec[Padding] = (constant(BitVector.low(length)) :: provide(length)).as[Padding]
}


/**
  * @param applicationId 32 bytes - Registered application ID. See https://xiph.org/flac/id.html
  * @param applicationData n bytes - Application data (n must be a multiple of 8)
  */
case class Application(applicationId: Long,applicationData: Vector[Byte]) extends Metadata

object Application {
  def baseCodec(applicationBlockLength: Int) = uint32 :: vectorOfN(provide(applicationBlockLength - 32), byte)

  /** Codec for Application
    * @param applicationBlockLength The length of this metadata block, defined in MetadataBlockHeader
    */
  def codec(applicationBlockLength: Int): Codec[Application] = baseCodec(applicationBlockLength).as[Application]
}

case class SeekPoint(sampleNumber: Long, offset: Long,numberOfSamples: Int)

object SeekPoint {
  val baseCodec = ulong(64) :: ulong(64) :: uint16
  val codec: Codec[SeekPoint] = baseCodec.as[SeekPoint]
}

/** Just an array of SeekPoint wrapped so we can claim it extends Metadata */
case class SeekTable(seekPoints: Vector[SeekPoint]) extends Metadata

object SeekTable {
  def numberOfSeekPoints(seekTableLength: Int) : Int = seekTableLength / (64 + 64 + 16) //size of SeekPoint

  def baseCodec(length: Int) = vectorOfN(provide(length / 8), SeekPoint.codec)

  def codec(length: Int) = baseCodec(length).as[SeekTable]

}

/**
  *
  * @param userComment this iteration's user comment = read a UTF-8 vector as [length] octets
  */
case class VorbisUserComment(userComment: String)

object VorbisUserComment {
  val baseCodec = variableSizeBitsLong(uint32L.xmap(_ * 8, _ / 8), utf8)
  val codec: Codec[VorbisUserComment] = baseCodec.as[VorbisUserComment]
}



/**
  *
  * @param vendorString read a UTF-8 vector as [vendorLength] octets
  * @param userComments array length should be size commentListLength
  */
case class VorbisComment(vendorString: String, userComments: Vector[VorbisUserComment]) extends Metadata

object VorbisComment {

  val baseCodec = variableSizeBitsLong(uint32L.xmap(_ * 8, _ / 8), utf8) :: vectorOfN(uint32L.xmap(_.toInt, _.toLong), VorbisUserComment.codec)

  val codec: Codec[VorbisComment] = baseCodec.as[VorbisComment]
}

case class CueSheetTrackIndex(offset: Long, indexPoint: Int)

object CueSheetTrackIndex {
  val baseCodec = ulong(64) :: uint8 :: constant(bin"000000000000000000000000")

  val codec: Codec[CueSheetTrackIndex] = baseCodec.as[CueSheetTrackIndex]
}

case class CueSheetTrack(trackOffset: Long, trackNumber: Int, isrc: String, audio: Boolean, preEmphasis: Boolean, trackIndexes: Vector[CueSheetTrackIndex])

object CueSheetTrack {

  val reservedStr = "0" * (6 + 13 * 8)
  val reservedBv = BitVector(reservedStr.getBytes)

  val baseCodec = ulong(64) :: uint8 :: fixedSizeBits(12 * 8, ascii) :: bool :: bool :: constant(reservedBv) :: vectorOfN( uint8, CueSheetTrackIndex.codec)

  val codec: Codec[CueSheetTrack] = baseCodec.as[CueSheetTrack]
}
/**
  *
  */
case class CueSheet(catalogNumber: String, leadInSamples: Long, correspondsToCd: Boolean, cueSheetTracks: Vector[CueSheetTrack]) extends Metadata

object CueSheet {
  val reserved = BitVector( ("0" * (7 + 258 * 8)).getBytes )
  val baseCodec = fixedSizeBytes(128*8, ascii) :: ulong(64) :: bool :: constant(reserved) :: vectorOfN(uint8, CueSheetTrack.codec)
  val codec: Codec[CueSheet] = baseCodec.as[CueSheet]
}

case class Picture(pictureType: Long, mime: String, description: String, pictureWidth: Long, pictureHeight:Long, colorDepth: Long,
                   numberOfColors: Long, binaryPicture: Vector[Byte]) extends Metadata


object Picture {

  val basecodec = uint32 :: variableSizeBitsLong(uint32.xmap(_ * 8, _ / 8), ascii) :: variableSizeBitsLong(uint32.xmap(_ * 8, _ / 8), utf8) ::
    uint32 :: uint32 :: uint32 :: uint32 :: vectorOfN(uint32.xmap(x => (x * 8).toInt, x => (x / 8).toLong), byte)

  val codec: Codec[Picture] = basecodec.as[Picture]
}








