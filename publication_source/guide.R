write_guide <- function(stage,placements,tables,rate_dir) {
  escape <- function(s) {
    s <- gsub("&","&amp;",s,fixed=TRUE)
    s <- gsub("<","&lt;",s,fixed=TRUE)
    s <- gsub(">","&gt;",s,fixed=TRUE)
    gsub('"',"&quot;",s,fixed=TRUE)
  }
  # Embed small previews so the guide can also be opened separately from the ZIP.
  base64 <- function(raw) {
    chars <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/","")[[1]]
    pad <- (3-length(raw)%%3)%%3
    b <- matrix(c(as.integer(raw),rep(0L,pad)),nrow=3)
    codes <- rbind(b[1,]%/%4,(b[1,]%%4)*16+b[2,]%/%16,(b[2,]%%16)*4+b[3,]%/%64,b[3,]%%64)
    out <- chars[codes+1L]
    if (pad) out[seq.int(length(out)-pad+1,length(out))] <- "="
    paste(out,collapse="")
  }
  # Build the article insert from the same Step 17 run as the displayed figure.
  main <- placements[placements$id=="V19",]
  weeks <- read.csv(file.path(rate_dir,"weekly_asmr.csv"),stringsAsFactors=FALSE)
  years <- read.csv(file.path(rate_dir,"annual_asmr.csv"),stringsAsFactors=FALSE)
  delta <- weeks[weeks$age_group=="15+" & weeks$week=="2021-W48",]
  year <- years[years$age_group=="15+" & years$iso_year==2023,]
  stopifnot(nrow(main)==1L,nrow(delta)==1L,nrow(year)==1L,year$weeks==52L)
  rate_cols <- c("original_no_dose_asmr","adjusted_no_dose_asmr","original_vaccinated_asmr","adjusted_vaccinated_asmr")
  stopifnot(all(is.finite(as.matrix(delta[rate_cols]))),all(is.finite(as.matrix(year[rate_cols]))))
  fmt <- function(n) formatC(n,format="f",digits=0,big.mark=",")
  move <- function(original,adjusted) {
    sprintf("would %s from %s to %s",if(adjusted>=original) "rise" else "fall",fmt(original),fmt(adjusted))
  }
  reversal <- (year$original_no_dose_asmr-year$original_vaccinated_asmr) *
              (year$adjusted_no_dose_asmr-year$adjusted_vaccinated_asmr) < 0
  paragraph <- paste(
    "Figure V19 compares the two dose groups at ages 15 and over using the same standard age distribution.",
    "Suppose that, within each five-year age band, 15% of deaths after a first dose were counted on the no-dose side over the full study, with ages 90 and over pooled.",
    sprintf("In week 48 of 2021, during the Delta wave, the no-dose ASMR %s deaths per 100,000 person-years, while the rate after a first dose %s.",
            move(delta$original_no_dose_asmr,delta$adjusted_no_dose_asmr),move(delta$original_vaccinated_asmr,delta$adjusted_vaccinated_asmr)),
    if (reversal) "For 2023 as a whole, the ordering would reverse." else "For 2023 as a whole, the ordering would be preserved.",
    sprintf("The no-dose rate %s and the rate after a first dose %s.",
            move(year$original_no_dose_asmr,year$adjusted_no_dose_asmr),move(year$original_vaccinated_asmr,year$adjusted_vaccinated_asmr)),
    "Both panels count the same resident deaths and use the same person-time. The actual frequency of missed links remains unknown.")
  main_alt <- paste("Weekly all-cause mortality at ages 15 and over by recorded dose status and for all residents.",
                    "Panels A and B compare the original allocation with the 15% missed-linkage scenario.",
                    "Panel C stacks first and later doses. Vertical lines mark sustained variant majorities in Czech sequences.")
  main_caption <- paste0("Figure V19. ",main$title,". ",main$caption)
  img <- file.path(stage,"png",main$file)
  standalone <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
    paste0("<title>",escape(main$subheading),"</title>"),
    "<style>body{max-width:54rem;margin:2rem auto;padding:0 1rem;color:#152e36;font:18px/1.55 Georgia,serif}h3{line-height:1.2}figure{margin:1.5rem 0}img{width:100%;height:auto}figcaption{font:15px/1.5 system-ui,sans-serif;margin-top:.5rem}</style></head><body><article>",
    paste0("<h3>",escape(main$subheading),"</h3><p>",escape(paragraph),"</p>"),
    paste0("<figure><img src='data:image/png;base64,",base64(readBin(img,"raw",n=file.size(img))),"' alt='",escape(main_alt),"'><figcaption>",escape(main_caption),"</figcaption></figure></article></body></html>"))
  writeLines(enc2utf8(standalone),file.path(stage,"V19_substack_section.html"),useBytes=TRUE)
  # Keep the article insert tied to the numbers displayed in the rebuilt PNGs.
  t3 <- subset(tables$T03$limits,iso_year==2023 & source=="november" & status=="no_dose")
  t5 <- subset(tables$T05$limits,age_group=="25-49" & allocation=="scenario")
  stopifnot(nrow(t3)==1L,nrow(t5)==2L)
  fmt_rate <- function(x) formatC(x,format="f",digits=1,big.mark=",")
  interval_text <- function(r) paste0(fmt_rate(r$rate)," (95% interval ",fmt_rate(r$lower95),"–",fmt_rate(r$upper95),")")
  copy_t03 <- paste0("The intervals describe how much the estimated rates could vary from counting deaths under a Poisson model. ",
    "For November records with no recorded dose and a known birth band, the 2023 estimate was ",
    interval_text(t3)," deaths per 100,000 person-years. The scenarios below examine ",
    "how the estimate changes when we change the ages or time at risk assigned to the records.")
  shifted <- t5$deaths[t5$status=="vaccinated"]-with(tables$T05$limits, deaths[age_group=="25-49" & allocation=="original" & status=="vaccinated"])
  copy_t05 <- paste0("At ages 25–49, the scenario reassigns a net ",formatC(shifted,format="f",digits=0,big.mark=",")," deaths in 2023. Pooling deaths and person-time across this age group gives rates of ",
    interval_text(t5[t5$status=="no_dose",])," with no recorded dose and ",
    interval_text(t5[t5$status=="vaccinated",])," after a first dose. Both rates use the same pool of resident deaths. ",
    "Each interval describes counting variation for one rate under the assumed allocation. A comparison of the two rates would also need to account for how those deaths were divided between them.")
  # Article copy can replace the existing numeric sentences rather than duplicating them.
  html <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>Counting intervals for Tables T03 and T05</title>",
    "<style>body{font:17px/1.55 system-ui,sans-serif;max-width:58rem;margin:2rem auto;padding:0 1rem;color:#183943}section{margin:2rem 0}img{width:100%;height:auto}figure{margin:1rem 0}aside{padding:.65rem 1rem;background:#edf4f5}h1,h2{line-height:1.2}</style></head><body><h1>Tables T03 and T05 · Substack edits</h1>")
  for (id in c("T03","T05")) {
    r <- placements[placements$id==id,]
    title <- paste0("Table ",id,". ",r$title,". ",r$caption)
    img_path <- file.path(stage,"png",r$file)
    html <- c(html,paste0("<section><h2>Table ",id,"</h2><aside>",escape(r$placement)," Replace the existing image and caption.</aside>",
      "<figure><img alt='",escape(title),"' src='data:image/png;base64,",base64(readBin(img_path,"raw",n=file.size(img_path))),"'><figcaption>",escape(title),"</figcaption></figure>"),
      if(id=="T03") "<aside>Insert after T03, before the paragraph beginning “After a first dose”.</aside>" else
        "<aside>After T05, keep the sentence giving the original rates. Replace the next sentence, beginning “Reassigning a net 423 deaths”, with the passage below. Then resume at “Against each group’s own seasonal expectation”.</aside>",
      paste0("<p>",escape(if(id=="T03") copy_t03 else copy_t05),"</p></section>"))
  }
  html <- c(html,"<h2>Methods wording for the existing notes</h2>",
    "<p><strong>Append to note 24.</strong> We calculated the ASMR interval from the annual deaths and person-time in each of the 16 attained-age bands using the Fay–Feuer gamma method and the same ESP 2013 weights as the point estimate. Fractionally allocated deaths enter the Poisson calculation unrounded. The limits approximate event-count variation with the denominators and allocation rules held fixed. <a href='https://wonder.cdc.gov/controller/pdf/FayFeuerConfidenceIntervals.pdf'>Fay and Feuer, 1997</a>.</p>",
    "<p><strong>Append to note 29.</strong> For Table T05 we pooled 2023 deaths and person-days within each displayed age group and dose status, separately for the original and scenario allocations. We used equal-tailed gamma limits on the allocated count divided by its person-years. Fractional counts make this a working approximation. These marginal intervals exclude uncertainty in linkage and person-time and do not test the difference between the two rates.</p></body></html>")
  writeLines(enc2utf8(html),file.path(stage,"T03_T05_substack_edits.html"),useBytes=TRUE)
  preview <- function(name) {
    im <- png::readPNG(file.path(stage,"png",name),native=TRUE)
    width <- min(620,ncol(im)); height <- round(nrow(im)*width/ncol(im))
    p <- tempfile(fileext=".png"); on.exit(unlink(p))
    png(p,width=width,height=height,bg="white")
    tryCatch(grid::grid.raster(im,width=1,height=1,interpolate=TRUE),finally=dev.off())
    paste0("data:image/png;base64,",base64(readBin(p,what="raw",n=file.size(p))))
  }
  data_table <- function(t) {
    hdr <- paste0("<th scope='col'>",gsub("\n"," ",escape(t$headers),fixed=TRUE),"</th>",collapse="")
    row <- function(i) paste0("<tr><th scope='row'>",escape(t$rows[i,1]),"</th>",
      paste0("<td>",escape(t$rows[i,-1]),"</td>",collapse=""),"</tr>")
    paste0("<details><summary>Read the table values</summary><div class='scroll'><table><thead><tr>",hdr,
           "</tr></thead><tbody>",paste(vapply(seq_len(nrow(t$rows)),row,""),collapse=""),"</tbody></table></div><p>",escape(t$note),"</p></details>")
  }
  css <- "html{scroll-behavior:smooth}*{box-sizing:border-box}body{margin:0;background:#f5f7f4;color:#18343b;font:16px/1.5 system-ui,sans-serif}main{max-width:1080px;margin:auto;padding:32px clamp(16px,4vw,50px) 80px}h1,h2,h3{line-height:1.2}h1{font:600 clamp(32px,5vw,54px)/1.08 Georgia,serif;margin:14px 0}h2{font:600 30px/1.2 Georgia,serif;margin:40px 0 14px}h3{font-size:21px;margin:0 0 6px}p{margin:6px 0 12px}nav{display:flex;gap:6px 16px;flex-wrap:wrap;border-block:1px solid #c4d2d1;padding:15px 0;margin:22px 0}a{color:#07586a}article{background:white;border:1px solid #d2dddc;border-radius:8px;padding:18px;margin:12px 0;display:grid;grid-template-columns:minmax(0,1.55fr) minmax(180px,1fr);gap:18px}article>div{min-width:0}article img{display:block;width:100%;height:auto;border:1px solid #dce5e4}article figure{margin:0;align-self:start}code{font:13px/1.4 ui-monospace,monospace;overflow-wrap:anywhere;color:#144954}small{color:#43616a}details{margin-top:12px}summary{cursor:pointer;color:#07586a}.scroll{overflow:auto}table{border-collapse:collapse;font-size:13px;margin:10px 0;width:100%}th,td{padding:5px;border-bottom:1px solid #c9d4d5;text-align:right}th:first-child{text-align:left}tr:nth-child(even){background:#f4f8f8}@media(max-width:680px){article{grid-template-columns:1fr}article figure{max-width:420px}}@media print{nav{display:none}article{break-inside:avoid}body{background:white}}"
  sections <- unique(placements$section)
  section_id <- function(i) paste0("section-",i)
  nav <- paste(vapply(seq_along(sections),function(i) sprintf("<a href='#%s'>%s</a>",section_id(i),
    if(grepl("^[0-9]",sections[i])) paste("Section",sub("\\..*","",sections[i])) else escape(sections[i])),""),collapse="")
  body <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>Czech mortality analysis · Substack image guide</title>",
    paste0("<style>",css,"</style></head><body><main>"),"<h1>Substack image placement guide</h1>",
    "<p>The revised article has 20 figures and nine tables. Insert the new ASMR subsection after V18 and before Section 8. Replace the historical peak image in the Methods appendix with V20, including its caption and alt text.</p>",
    "<p><a href='V19_substack_section.html'>Open the new subsection and Figure V19</a>. Use the original PNG in the editor. All 29 placements are listed below in reading order. The guide gives the upload filename, caption and preview for each.</p>",
    "<p><a href='T03_T05_substack_edits.html'>Article copy and updated tables with counting intervals</a> · <a href='T03_T05_rate_intervals.csv'>Download all 32 estimates and limits</a>.</p>",
    paste0("<nav aria-label='Sections'>",nav,"</nav>"))
  for (i in seq_along(sections)) {
    section <- sections[i]
    body <- c(body,sprintf("<section id='%s'><h2>%s</h2>",section_id(i),escape(section)))
    if (section=="Supplementary figures") body <- c(body,"<p>S01–S03 cover the original comparisons. S05–S10 give separate age-group views of the missed-linkage scenario shown in main-text Figure V19.</p>")
    for (j in which(placements$section==section)) {
      r <- placements[j,];id <- r$id
      kind <- switch(substr(id,1,1),T="Table",V="Figure",S="Supplement")
      name <- paste(kind,id);caption <- paste0(name,". ",r$title,". ",r$caption)
      credit <- if(r$source=="ČSÚ G.02") "ČSÚ Demographic Yearbooks, Table G.02 (2020–2023)" else paste(if(grepl(" and ",r$source,fixed=TRUE)) "Steps" else "Step",r$source)
      body <- c(body,paste0("<article id='",id,"'><div><h3>",escape(name)," · ",escape(r$title),"</h3>",
        if(nzchar(r$subheading)) paste0("<p><small>",escape(r$subheading),"</small></p>") else "",
        "<p>",escape(r$placement),"</p>",
        if (id=="V19") paste0("<p><a href='V19_substack_section.html'>Copy the subsection</a></p><p>",escape(paragraph),"</p>") else "",
        "<p><a href='png/",r$file,"' download><code>",r$file,"</code></a></p>",
        "<p><strong>Suggested caption</strong><br>",escape(caption),"</p><small>Source · ",escape(credit),"</small>",
        if(id %in% names(tables)) data_table(tables[[id]]) else "",
        "</div><figure><a href='png/",r$file,"'><img alt='",escape(if(id=="V19") main_alt else paste(name,r$title)),"' loading='lazy' src='",preview(r$file),"'></a></figure></article>"))
    }
    body <- c(body,"</section>")
  }
  writeLines(enc2utf8(c(body,"</main></body></html>")),file.path(stage,"substack_insertion_guide.html"),useBytes=TRUE)
}
