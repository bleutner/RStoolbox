library(methods)
library(devtools)
library(stringr)
library(knitr)
library(raster)
library(RStoolbox)

knit_rd2 <- function(pkg, path = ".", links =  tools::findHTMLlinks(), frame = FALSE, cdr = FALSE, copycss=FALSE) {
    opts_chunk$set(comment="#>")
    force(oldworki <- getwd())
    setwd(path) ; on.exit(setwd(oldworki))
    library(pkg, character.only = TRUE)
    
    optc = opts_chunk$get(); on.exit(opts_chunk$set(optc), add=T)
    if(copycss) file.copy(system.file('misc', 'R.css', package = 'knitr'), './')
    pkgRdDB = getFromNamespace('fetchRdDB', 'tools')(file.path(find.package(pkg), 'help', pkg))
    
    force(links); topics = names(pkgRdDB)
    for (topici in topics) { 
        message('** knitting documentation of ', topici)
        tools::Rd2HTML(pkgRdDB[[topici]], f <- tempfile(),
                package = pkg, Links = links, no_links = is.null(links), stages = 'render')
        txt = readLines(f, warn = FALSE)
        extlinks <- grep("^.*\\.\\./\\.\\./", txt)
        
        included <- c("/base/", "/boot/", "/class/", "/cluster/", "/codetools/", 
                "/compiler/", "/datasets/", "/foreign/", "/graphics/", "/grDevices/", 
                "/grid/", "/KernSmooth/", "/lattice/", "/MASS/", "/Matrix/", 
                "/methods/", "/mgcv/", "/nlme/", "/nnet/", "/parallel/", "/rpart/", 
                "/spatial/", "/splines/", "/stats/", "/stats4/", "/survival/", 
                "/tcltk/", "/tools/", "/utils/")
        
        txt[extlinks] <- gsub(".html", "", gsub("/html/", "/docs/", gsub("../../", "http://www.inside-r.org/packages/cran/", txt[extlinks])))
        extIntLinks <- sapply(included, function(x) any(grepl(x, txt[extlinks])))
        ints <- included[extIntLinks]
        if(length(ints)) for(i in ints) {
                txt[extlinks] <- gsub(paste0("/packages/cran", i, "docs/"), paste0("/r-doc/stats/"), txt[extlinks])
            }
        
        
        if (length(i <- grep('<h3>Examples</h3>', txt)) == 1L &&
                length(grep('</pre>', txt[i:length(txt)]))) {
            i0 = grep('<pre>', txt); i0 = i0[i0 > i][1L] - 1L
            i1 = grep('</pre>', txt); i1 = i1[i1 > i0][1L] + 1L
            
            tools::Rd2ex(pkgRdDB[[topici]], ef <- tempfile(), commentDontrun=cdr)
            ex = readLines(ef, warn = FALSE)
            ex = ex[-(1L:grep('### ** Examples', ex, fixed = TRUE))]
            ex = c('```{r}', ex, '```')
            
           
            noru <- grep("##D", ex)
            ex[noru] <- gsub("##D", "##", ex[noru])
            
            opts_chunk$set(fig.path = str_c('figure/', topici, '-'),  tidy = FALSE)
            res = try(knit2html(text = ex, envir = parent.frame(2), fragment.only = TRUE, quiet = TRUE))
            if (inherits(res, 'try-error')) {
                res = ex; res[1] = '<pre><code class="r">'; res[length(res)] = '</code></pre>'
            }
            txt = c(txt[1:i0], res, txt[i1:length(txt)])
            txt = sub('</head>', '
                            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/styles/github.min.css">
                            <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/highlight.min.js"></script>
                            <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/languages/r.min.js"></script>
                            <script>hljs.initHighlightingOnLoad();</script>
                            </head>', txt)            
        } else message('no examples found for ', topici)
        title <- txt[grep("<head><title>", txt) ]
        title <- gsub("^.*<head><title>R: |</title>", "", title)
        
        H <- grep("<table width=|<h3>Description", txt)
        txt <- txt[-(H[1]:(H[2]-1))] 
        
        ## Fix bottom index link
        indlink <- grep(">Index</a>", txt)
        txt[indlink] <- gsub("00Index.html", "index.html", txt[indlink])
        
        ## Remove dontshow sections
        nosho <- grep("## Don&#39;t show:", txt)
        if(length(nosho)){
            su <- txt[nosho]
            fro <- str_locate( su, "\\n\\n## Don&#39;t show:")
            fre <- str_locate( su, "## End\\(Don&#39;t show\\)")
            for(i in 1:nrow(fre)){
               su <- paste0(substr(su, 1, fro[i,1]-1), substr(su, fre[i,2]+1, nchar(su))) 
            }
            txt[nosho] <- su
        }
        
        ## Tidy up don't run section
#        nosho <- grep("\\#\\# Not run:", txt)
#        if(length(nosho)){
#            su <- txt[nosho]
#            fro <- str_locate( su, "\\#\\# Not run: \\n\\#\\#D")
#            fre <- str_locate( su, "\\#\\# End\\(Not run\\)")
#            for(i in 1:nrow(fre)){             
#                substr(su, fro[i,2], fro[i,2])<-" "
#            }
#            txt[nosho] <- su
#        }
        
         
        ## Concatenate lines
        txt <- paste("---\nlayout: docu\ntitle: '",title,"'\nfun: ", topici ,"\npackage: ", pkg,
                "\nheader: Pages\ngroup: navigation\n---\n{% include JB/setup %}\n", paste0(txt, collapse ="\n"))
        
        
        writeLines(txt, str_c(topici, '.html'))
    }
    unlink('figure/', recursive = TRUE)
    toc = sprintf('- <a href="%s" target="content">%s</a>', str_c(topics, '.html'), topics)
    markdown::markdownToHTML(text = paste(toc, collapse = '\n'), output = '00frame_toc.html',
            title = paste('R Documentation of', pkg),
            options = NULL, extensions = NULL, stylesheet = 'R.css')
    txt = readLines(file.path(find.package(pkg), 'html', '00Index.html'))
    unlink('00Index.html')
    # fix image links
    
    index <- gsub('../../../doc/html/', 'http://stat.ethz.ch/R-manual/R-devel/doc/html/',txt, fixed = TRUE)	
    ir <- grep("</head><body>|</div><h2>Documentation for package", index)
    index <- index[-((ir[1]+1):ir[2])]
    
    index <- index[-(grep("DESCRIPTION file</a>", index)+0:2)]
    
    index <- paste0("---\nlayout: page\ntitle: Package Documentation\nheader: Pages\ngroup: navigation\n---\n{% include JB/setup %}\n",
            paste(index, collapse="\n"))
    writeLines(index, 'index.html')
    
    
    
}

knit_rd2(pkg = "RStoolbox", path = "rstbx-docu", cdr = TRUE)

