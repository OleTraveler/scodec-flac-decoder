package com.ot.flac.decoder

import scodec.{Codec, Decoder}
import scodec.codecs._

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
    val codec = header.blockType match {
      case STREAMINFO => StreamInfo.codec
      case PADDING => Padding.codec
      case APPLICATION => Application.codec(header.length)
      case SEEKTABLE => SeekTable.codec(header.length)
      case VORBIS_COMMENT => VorbisComment.codec
      case CUESHEET => CueSheet.codec
      case PICTURE => Picture.codec
      case _ => sys.error("Invalid")
    }
    codec.map(c => (header,c))
  }

  def hc: Decoder[(MetadataBlockHeader, Metadata, Vector[(MetadataBlockHeader, Metadata)])] = MetadataBlockHeader.codec.flatMap(blockHeaderToMetadataSection)



}

object MetadataBlockHeader {
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
  md5: Array[Byte]
) extends Metadata

object StreamInfo {
  val baseCodec = uint16 :: uint16 :: uint24 :: uint24 :: uint(20) :: uint(3) :: uint(5) :: ulong(36) :: byte(128)
  val codec: Codec[StreamInfo] = baseCodec.as[StreamInfo]
}


/**
  * n '0' bits (n must be a multiple of 8)
  * @param n number of Byte: 0's.
  */
case class Padding(n: Long) extends Metadata

object Padding {
  import scodec.bits._
  val baseCodec = vector(constant(bin"00000000")).xmap[Long](_.size, l => (1 to l).map(_ => ()).toVector)

  val codec: Codec[Padding] = baseCodec.as[Padding]
}


/**
  * @param applicationId 32 bytes - Registered application ID. See https://xiph.org/flac/id.html
  * @param applicationData n bytes - Application data (n must be a multiple of 8)
  */
case class Application(applicationId: Int,applicationData: Array[Byte]) extends Metadata

object Application {
  def baseCodec(applicationBlockLength: Int) = uint32 :: byte(applicationBlockLength - 8)

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
case class SeekTable(seekPoints: List[SeekPoint]) extends Metadata

object SeekTable {
  def numberOfSeekPoints(seekTableLength: Int) : Int = seekTableLength / (64 + 64 + 16) //size of SeekPoint

  def baseCodec(length: Int) = sizedVector(length / 8, SeekPoint.codec)

  def codec(length: Int) = baseCodec(length).as[SeekTable]

}

/**
  *
  * @param length read an unsigned integer of 32 bits
  * @param userComment this iteration's user comment = read a UTF-8 vector as [length] octets
  */
case class VorbisUserComment(length: Int, userComment: String)

object VorbisUserComment {
  val baseCodec = utf8_32L
  val codec: Codec[VorbisUserComment] = baseCodec.as[VorbisUserComment]
}



/**
  *
  * @param vendorLength read an unsigned integer of 32 bits
  * @param vendorString read a UTF-8 vector as [vendorLength] octets
  * @param commentListLength read an unsigned integer of 32 bits
  * @param userComments array length should be size commentListLength
  * @param framingBit read a single bit as boolean
  */
case class VorbisComment(vendorLength: Int, vendorString: String, commentListLength: Int,
                         userComments: Array[VorbisUserComment], framingBit: Boolean) extends Metadata

object VorbisComment {

  //TODO: Need to fix this.
  val uint32L2Int = uint32L.xmap[Int](l => {
    if (l < 0 || l > Int.MaxValue) {
      sys.error("Need to implement vectorOfN to take Codec[Long]")
    } else {
      l.toInt
    }
  }, { i => i.toLong })

  val baseCodec = utf8_32L :: vectorOfN(uint32L2Int, VorbisUserComment.codec)
  val codec: Codec[VorbisComment] = baseCodec.as[VorbisComment]
}

case class CueSheetIndex(offset: Long, index: Byte)

object CueSheetIndex {
  val baseCodec = ulong(64) :: uint8 :: fixedSizeBits(3*8, bytes)
  val codec: Codec[CueSheetIndex] = baseCodec.as[CueSheetIndex]
}

case class CueSheetTrack(trackOffset: Long, trackNumber: Byte, isrc: String, audio: Boolean, preEmphasis: Boolean,
                         trackIndexPoints: Byte)

object CueSheetTrack {
  val baseCodec = ulong(64) :: uint8 :: fixedSizeBits(12 * 8, ascii) :: bits(1) :: bits(1) ::
    fixedSizeBits(6 + 13 * 8, bytes) :: vectorOfN( uint8, CueSheetIndex.codec)

  val codec: Codec[CueSheetTrack] = baseCodec.as[CueSheetTrack]
}
/**
  *
  */
case class CueSheet(catalogNumber: String, leadInSamples: Long, correspondsToCd: Boolean, reserved: Array[Byte], numberOfTracks: Int,
                    cueSheetTracks: Array[CueSheetTrack]) extends Metadata

object CueSheet {
  val baseCodec = fixedSizeBytes(128*8, ascii) :: ulong(64) :: bool :: bytes(7 + 258 * 8) :: vectorOfN(uint8, CueSheetTrack.codec)
  val codec: Codec[CueSheet] = baseCodec.as[CueSheet]
}

case class Picture(pictureType: Short, mime: String, description: String, pictureWidth: Long, pictureHeight:Long, colorDepth: Long,
                   numberOfColors: Long, binaryPicture: Array[Byte]) extends Metadata


object Picture {
  val longTimesEight: Codec[Int] = uint32.xmap[Int](l =>  (l * 8).toInt, i => (i / 8).toLong)

  val basecodec = ushort(32) :: variableSizeBits(longTimesEight, ascii) :: variableSizeBits(longTimesEight, utf8) ::
    uint32 :: uint32 :: uint32 :: uint32 :: uint32 :: variableSizeBits(longTimesEight, bytes)

  val codec: Codec[Picture] = basecodec.as[Picture]
}








