package org.fmindex

object IndexerAppCache extends optional.Application {
    def main() {
        val r = new FileBWTReader("testdata/include.cache")
        val bm = new BWTMerger2(1024*1024*10,debugLevel=2)
        val (of,af) = bm.merge(r)
    }
}

object FMCreatorApp extends optional.Application {
    def main(file:String) {
        val bm = new FMCreator(file)
        bm.create()
    }
}

object IndexerApp extends optional.Application {
  def main(dir: String = "/usr/include/",i:Int=10) {
    val r = new DirBWTReader(dir,"testdata/include",debugLevel=2,caching=true)
    
    //val r = new FileBWTReader("testdata/include.cache")
    
    //val r = new DirBWTReader("/data_nb/aleh/MProg/findex/src","testdata/include",debugLevel=0,caching=true)
    val bm = new BWTMerger2(1024*1024*i,debugLevel=2)
    val (of,af) = bm.merge(r)
  }
}
