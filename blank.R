xs <- bd.coerce(cox[, c(
        grep("online.*exp", names(cox), value=TRUE),
        grep("vlist", names(cox), value=TRUE),
        "campaign")])
xs <- xs[xs$campaign == "rp13.q1"]

t <- table(xs$vlist, as.numeric(xs$rp13.q1.online.total.overall.exp > 0))
total.respondents <- sum(t)
total.respondents.online.exp <- sum(t[, 2])
