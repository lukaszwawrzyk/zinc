logLevel := Level.Debug
classDirectory in Compile := {
  val jar = (artifactPath in (Compile, packageBin)).value
  sbt.IO.createDirectory(jar.getParentFile)
  jar
}
