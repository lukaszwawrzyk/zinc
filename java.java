
  // puts all files in `from` (overriding the original files in case of conflicts)
  // into `to`, removing `from`. In other words it merges `from` into `into`.
  public static void mergeJars(File into, File from) {
    java.util.HashMap<String, String> env = new java.util.HashMap<String, String>()
    FileSystem intoFs = FileSystems.newFileSystem(fileToJarUri(into), env)
    FileSystem fromFs = FileSystems.newFileSystem(fileToJarUri(from), env)
    try {
      Files
        .walk(fromFs.getPath("/"))
        .forEachOrdered(new Consumer[Path] {
           public void accept(Path t) {
            if (Files.isDirectory(t)) {
              Files.createDirectories(intoFs.getPath(t.toString))
            } else {
              Files.copy(t,
                intoFs.getPath(t.toString),
                StandardCopyOption.COPY_ATTRIBUTES,
                StandardCopyOption.REPLACE_EXISTING)
            }
          }
        })
    } finally {
      intoFs.close()
      fromFs.close()
    }
    from.delete()
  }

  private static URI fileToJarUri(File jarFile) {
    new URI("jar:" + jarFile.toURI().toString())
  }
