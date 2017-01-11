wtd.means = by(firms, size.category,
  function(piece) (sum(piece$mktcap*piece$spread)/sum(piece$mktcap)))