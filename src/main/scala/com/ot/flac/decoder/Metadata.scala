package com.ot.flac.decoder

import scala.collection.BitSet

/**
  * Created by tstevens on 4/6/17.
  */
trait Metadata {

}

object MetadataBockHeader {
  val STREAMINFO: Byte = 0
  val PADDING: Byte = 1
  val APPLICATION: Byte = 2
  val SEEKTABLE: Byte = 3
  val VORBIS_COMMENT: Byte = 4
  val CUESHEET: Byte = 5
  val PICTURE: Byte = 6
  //7-126 : reserved
  val INVALID: Byte = 127  //invalid, to avoid confusion with a frame sync code
}

/**
  *
  * @param lastMetadataBlock Last-metadata-block flag: '1' if this block is the last metadata block before the audio blocks, '0' otherwise.
  * @param blockType BLOCK_TYPE
  * @param length Length (in bytes) of metadata to follow (does not include the size of the METADATA_BLOCK_HEADER)
  */
case class MetadataBockHeader(
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
  md5: BigInt
) extends Metadata

/**
  * n '0' bits (n must be a multiple of 8)
  * @param n number of Byte: 0's.
  */
case class Padding(n: Long) extends Metadata

/**
  *
  * @param applicationId 32 bytes - Registered application ID. See https://xiph.org/flac/id.html
  * @param applicationData n bytes - Application data (n must be a multiple of 8)
  */
case class Application(applicationId: Int,applicationData: Array[Byte]) extends Metadata

case class SeekPoint(sampleNumber: Long, offset: Long,numberOfSamples: Int)

/** Just an array of SeekPoint wrapped so we can claim it extends Metadata */
case class SeekTable(seekPoints: List[SeekPoint]) extends Metadata

/**
  *
  * @param length read an unsigned integer of 32 bits
  * @param userComment this iteration's user comment = read a UTF-8 vector as [length] octets
  */
case class VorbisUserComment(length: Int, userComment: String)

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

case class CueSheetIndex(offset: Long, index: Byte)
case class CueSheetTrack(trackOffset: Long, trackNumber: Byte, isrc: String, audio: Boolean, preEmphasis: Boolean,
                         trackIndexPoints: Byte)
/**
  *
  */
case class CueSheet(catalogNumber: String, leadInSamples: Long, correspondsToCd: Boolean, numberOfTracks: Int,
                    cueSheetTracks: Array[CueSheetTrack]) extends Metadata

case class Picture(pictureType: Short, mimeLength: Int, mime: String, decriptionLength: Int, description: String,
                   pictureWidth: Int, pictureHeight:Int, colorDebth: Int, numberOfColors: Int, pictureLength: Int,
                  binaryPicture: Array[Byte]) extends Metadata








