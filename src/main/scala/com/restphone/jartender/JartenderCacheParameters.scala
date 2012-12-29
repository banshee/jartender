package com.restphone.jartender

case class JartenderCacheParameters(
  cacheDir: String, // Stores the cached jar files and the text files used to calculate signatures.  APS can also clear it on clean builds, so it should be a dedicated directory.
  classFiles: List[String], // Files containing application code, directories and/or jar files; used in proguard -injar option.  The class files for your application go here.
  inputJars: List[String], // Written to the proguard file as -injar options, before -outjar.  (Order in the proguard file is what makes only scala libraries appear in the output.)
  libraryJars: List[String] = List() // passed to proguard -libraryjars.  Should always include android.jar.
  )
