object IndexMaker {
    import java.io.File
    import org.apache.commons.io.IOUtils
    def recursiveListFiles(f: File): Stream[File] = {
        var these = f.listFiles
        if ( these == null )
            Stream.empty
        else
            these.filter(! _.isDirectory).toStream append these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    def packTo(buf:java.io.ByteArrayOutputStream,f:File) = {
        val fs = new java.io.FileInputStream(f)
        IOUtils.copy(fs,buf)
    }
    def make(dir:String,recursive:Boolean=true) = {
        val files = recursiveListFiles(new File(dir))
        val buf = new java.io.ByteArrayOutputStream()
        files foreach { f:File  => packTo(buf,f) } 
    }
}