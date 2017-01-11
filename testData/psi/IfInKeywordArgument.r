a("\\cite" =
                   if (inCodeBlock) writeContent(block, tag)
                   else writeWrapped(block, tag))