package com.restphone.jartender

import java.io.File
import java.io.IOException

import scala.Array.fallbackCanBuildFrom
import scala.PartialFunction.condOpt
import scala.Predef.Set.apply

import com.google.common.base.Charsets
import com.google.common.io.Files
import com.restphone.jartender.FileFailureValidation._
import com.restphone.jartender.RichFile.stringToFile
import com.restphone.jartender.RichFile.tree

import CacheEntry.apply
import scalaz._
import scalaz.Failure.apply
import scalaz.Scalaz._
import scala.language.reflectiveCalls

sealed abstract class CacheResponse {
  def c: CacheEntry
}
case class ExistingLibrary( p: JartenderCacheParameters, c: CacheEntry ) extends CacheResponse
case class BuiltLibrary( p: JartenderCacheParameters, c: CacheEntry ) extends CacheResponse

case class UsersAndProviders( providers: Set[ProvidesElement], users: Set[UsesElement], providerFiles: ProviderFilesInformation )

case class CacheSystem( initialCache: Cache = new Cache( Set() ) ) {
  def execute( shrinker: Shrinker ): FailureValidation[CacheResponse] = {
    // A success on findInCache just means that we didn't throw an
    // exception - it still could be a miss.  Missing is fine; we can continue.
    // An exception isn't fine; we need to stop and report the exception.
    findInCache( shrinker.jartenderCacheParameters ) match {
      // converting to a different kind of failure
      case scalaz.Failure( x ) => Failure[NonEmptyList[FileFailureValidation.AbstractFailure], CacheResponse]( x ) 
      case Success( Some( x ) ) => x.success
      case Success( None ) => createMissingEntry( shrinker )
    }
  }

  def createMissingEntry( shrinker: Shrinker ): FailureValidation[BuiltLibrary] = {
    for {
      cachedJar <- shrinker.execute()
      newCacheEntry <- cacheEntryForProcessedLibrary( shrinker.jartenderCacheParameters, cachedJar )
      _ <- addCacheEntry( newCacheEntry )
    } yield BuiltLibrary( shrinker.jartenderCacheParameters, newCacheEntry )
  }

  def installOutputJar( cachedJar: File, destination: File ): FailureValidation[File] =
    convertExceptions( "installing output jar", destination ) {
      if ( !destination.exists || !Files.equal( cachedJar, destination ) ) {
        Files.copy( cachedJar, destination )
      }
      destination.success
    }

  def findInCache( p: JartenderCacheParameters ): FailureValidation[Option[ExistingLibrary]] =
    buildUsersAndProviders( p ) map { x =>
      val relevantDependencies = DependencyAnalyser.buildMatchingDependencies( x.providers, x.users )
      val cacheEntry = currentCache.findInCache( relevantDependencies, x.providerFiles )
      cacheEntry map { ExistingLibrary( p, _ ) }
    }

  def buildUsersAndProviders( p: JartenderCacheParameters ) = {
    ( elementsFromClassfiles( p )
      |@| elementsFromInputFiles( p )
      |@| providerFiles( p ) ) {
        ( elementsUsed, elementsProvided, providerFiles ) =>
          val providers = elementsProvided collect { case x: ProvidesElement => x }
          val users = elementsUsed collect { case x: UsesElement => x }
          val relevantDependencies = DependencyAnalyser.buildMatchingDependencies( providers.toSet, users.toSet )
          UsersAndProviders( providers = providers.toSet, users = users.toSet, providerFiles = providerFiles )
      }
  }

  def providerFiles( p: JartenderCacheParameters ) = ProviderFilesInformation.createFromFiles( p.inputJars map stringToFile )

  def addCacheEntry( c: CacheEntry ): FailureValidation[Cache] = {
    currentCache = new Cache( currentCache.entries + c )
    currentCache.success
  }

  def cacheEntryForProcessedLibrary( p: JartenderCacheParameters, f: File ): FailureValidation[CacheEntry] =
    buildUsersAndProviders( p ) map { x => CacheEntry( usesItems = x.users, providerFileInformation = x.providerFiles, jarfilepath = f.getPath ) }

  def elementsFromClassfiles( p: JartenderCacheParameters ) = elementsFromFiles( jvmFilesInDirectories( p.classFiles ) )
  def elementsFromInputFiles( p: JartenderCacheParameters ) = elementsFromFiles( jvmFilesInDirectories( p.inputJars ) )

  def jvmFilesInDirectories( classfiledirectories: Traversable[String] ) = for {
    classfileDirAsString <- classfiledirectories
    classfiledir = new File( classfileDirAsString )
    IsJvmFile( cf ) <- tree( classfiledir )
  } yield cf

  def elementsFromFiles( fs: Traversable[File] ): FailureValidation[List[ClassfileElement]] =
    ( fs.toList map elementsFromFile ).suml

  def elementsFromFile( f: File ): FailureValidation[List[ClassfileElement]] = {
    DependencyAnalyser.buildItemsFromFile( f ) map { _.elements }
  }

  def classesDefined( fs: Traversable[File] ): FailureValidation[List[JavaIdentifier]] =
    elementsFromFiles( fs ) map { _ collect { case ProvidesClass( _, _, internalName, _, _, _ ) => internalName.javaIdentifier } }

  private def fileContentsOrExceptionMessage( f: File ) = {
    try {
      Files.toString( f, Charsets.UTF_8 )
    } catch {
      case e: IOException => f"# ${e.getLocalizedMessage}".replace( '\n', ' ' )
    }
  }

   @transient private[jartender] lazy val IsJvmFile = new Object {
    val jvmExtensions = Set( "class", "jar" )
    def unapply( f: File ) = condOpt( f.getName.split( '.' ).reverse.toList ) {
      case h :: t if jvmExtensions.contains( h.toLowerCase ) && f.isFile => f
    }
  }

  var currentCache: Cache = new Cache( Set() )
}
