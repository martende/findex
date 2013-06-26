package org.fmindex


object IndexerApp extends optional.Application {
  def main() {
    val r = new DirBWTReader("/usr/include/","testdata/include",debugLevel=2,caching=true)
    
    //val r = new FileBWTReader("testdata/include.cache")
    
    //val r = new DirBWTReader("/data_nb/aleh/MProg/findex/src","testdata/include",debugLevel=0,caching=true)
    val bm = new BWTMerger2(1024*1024*10,debugLevel=2)
    val (of,af) = bm.merge(r)
  }
}
