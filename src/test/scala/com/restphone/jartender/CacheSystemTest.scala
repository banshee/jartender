package com.restphone.jartender

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.restphone.scalatestutilities.ScalaTestMatchers.beOfType
import com.google.common.io.Files
import java.io.File
import com.google.common.base.Charsets
import scala.util.control.Exception._
import java.io.IOException
import java.io.ObjectStreamException

class CacheSystemTest extends FunSuite with ShouldMatchers {
  test( "can serialize a CacheSystem" ) {
    val cs = new CacheSystem()
    cs.IsJvmFile
    val bytes = SerializableUtilities.convertToByteArray( cs )
    bytes should beOfType[Array[Byte]]
    val x = SerializableUtilities.byteArrayToObject[CacheSystem]( bytes )
    x should beOfType[Some[CacheSystem]]
    x.get.IsJvmFile should not be ( null )
  }

  test( "can generate cache file" ) {
    val tmpdir = Files.createTempDir
    val file1 = new File( tmpdir, "one.jar" )
    Files.write( "from", file1, Charsets.UTF_8 )

    val cachefile = new File( tmpdir, "cache.1.cache" )

    val usesSnark = UsesClass( JavaIdentifier( "com.restphone.Snark" ) )
    val usesBoojum = UsesClass( JavaIdentifier( "com.restphone.Boojum" ) )

    val pfi = ProviderFilesInformation.createFromFiles( List( file1 ) )
    pfi should be( 'success )

    val cachent = CacheEntry( usesItems = Set( usesSnark, usesBoojum ),
      providerFileInformation = pfi.toOption.get,
      jarfilepath = file1.getPath )

    val cache = Cache( Set( cachent ) )

    val cacheSystem = new CacheSystem( cache )

    val bytesForCache = SerializableUtilities.convertToByteArray( cacheSystem )

    Files.write( bytesForCache, cachefile )

    val bytesFromFile = Files.toByteArray( cachefile )

    val tst: Option[CacheSystem] = SerializableUtilities.byteArrayToObject( bytesFromFile )
    tst.get should beOfType[CacheSystem]
  }

}