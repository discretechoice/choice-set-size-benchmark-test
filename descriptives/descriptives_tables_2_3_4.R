################################################################################
#
# R script accompanying the paper:
#
# Procedural Invariance in Environmental Valuation:
# Are Preferences Stable Across Choice-Set Sizes?
#
# Anonymous replication materials
#
# Script:       descriptives_tables_2_3_4.R
# Purpose:      Reproduce Tables 2--4 using the cleaned MNL estimation sample.
# Manuscript:   Tables 2--4
#
# Inputs:
#   PrefMatWideFormatFullBiogeme.txt (repository root).
#
# Sample and data preparation:
#   Retain treatments 1--5, tasks 1--8, and education code v375 != 6.
#   Use one row per respondent for demographic and serial-choice summaries.
#
# Outputs:
#   descriptives/table2_*.csv, table3_*.csv and table4_*.csv
#   descriptives/sample_audit.csv and session_info.txt
#
# How to run:
#   From the repository root: Rscript descriptives/descriptives_tables_2_3_4.R
#
# Requirements:
#   R; reference environment: R 4.6.1.
#   Base R only.
#   See README.md for reference package versions.
#
# Reproducibility:
#   Random seed: Not applicable
#   Approximate runtime: Under 1 minute (planning estimate; base-R summaries).
#   Timing basis: Planning estimate; no model fitting.
#
################################################################################

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep('^--file=', args_all, value = TRUE)
if (!length(script_arg)) stop('Run this script using Rscript.')
script_dir <- dirname(normalizePath(sub('^--file=', '', script_arg[1])))
github_dir <- dirname(script_dir)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[1] else script_dir
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
raw <- read.table(file.path(github_dir, 'PrefMatWideFormatFullBiogeme.txt'), header = TRUE)
required <- c('id','treat','task','choibio','v371','v372','v375','v1')
stopifnot(all(required %in% names(raw)), !anyNA(raw[required]))
# Same selection and demographic definitions as estimations/MNL_TableA1/MNL_*alt.R.
# Positive indexing avoids the -which(logical(0)) empty-data edge case.
d <- raw[raw$treat %in% 1:5 & raw$task <= 8 & raw$v375 != 6, ]
d$age <- 2016 - d$v371
d$female <- as.integer(d$v372 == 1) # MNL also codes v372 == 0 as female=0.
# These two mappings reproduce the original Table 2 on the unfiltered sample.
# No questionnaire/codebook was supplied; inferred from all five original means.
d$university <- as.integer(d$v375 == 4)
d$visited <- as.integer(d$v1 == 1)
d <- d[order(d$treat, d$id, d$task), ]
stopifnot(!anyDuplicated(d[c('id','task')]), all(d$task %in% 1:8),
          all(d$choibio >= 1 & d$choibio <= d$treat + 1))
panels <- split(d, d$id)
stopifnot(all(vapply(panels, function(x) nrow(x)==8 &&
  identical(sort(x$task), 1:8) && all(vapply(x[c('treat','age','female','university','visited')],
  function(y) length(unique(y)) == 1, logical(1))), logical(1))))
u <- d[!duplicated(d$id), ]
u$sq_choices <- vapply(as.character(u$id), function(id) sum(panels[[id]]$choibio == 1), integer(1))
# Validate choibio's 1-based SQ coding against the original choice indicators.
choices <- as.matrix(d[paste0('choice',0:5)])
stopifnot(all(rowSums(choices)==1), all(choices[cbind(seq_len(nrow(d)),d$choibio)]==1))
labels <- paste0(2:6,'Alt')
N <- as.integer(table(factor(u$treat, levels=1:5)))
stopifnot(identical(N,c(305L,301L,305L,304L,281L)))
write_out <- function(x, name) write.csv(x, file.path(out_dir,name), row.names=FALSE, na='')
# Full precision statistics and sample standard deviations, one row per person.
vars <- c('age','female','university','visited')
t2 <- do.call(rbind,lapply(1:5,function(tr) do.call(rbind,lapply(vars,function(v) {
 x<-u[u$treat==tr,v]
 data.frame(treatment=labels[tr],variable=v,n=length(x),mean=mean(x),sd=sd(x))
}))))
write_out(t2,'table2_statistics.csv')
t2display <- data.frame(Characteristic=c('Age (years)','Gender (female = 1)',
 'Education (university = 1)','Visited Baltic Sea in last 12 months (yes = 1)','Respondents'),check.names=FALSE)
for(tr in 1:5) {
 z<-t2[t2$treatment==labels[tr],]
 t2display[[labels[tr]]]<-c(sprintf('%.2f (%.2f)',z$mean,z$sd),as.character(N[tr]))
}
write_out(t2display,'table2_display.csv')
# Omnibus ANOVA and all 10 pairwise pooled-variance t tests with Bonferroni correction.
an <- summary(aov(age ~ factor(treat),data=u))[[1]]
tests <- data.frame(variable='age',test='One-way ANOVA',statistic=an[1,'F value'],
 df1=an[1,'Df'],df2=an[2,'Df'],p_value=an[1,'Pr(>F)'])
for(v in vars[-1]) {
 ct<-chisq.test(table(u$treat,u[[v]]),correct=FALSE)
 tests<-rbind(tests,data.frame(variable=v,test='Pearson chi-squared',statistic=unname(ct$statistic),
 df1=unname(ct$parameter),df2=NA,p_value=ct$p.value))
}
write_out(tests,'table2_tests.csv')
pw<-pairwise.t.test(u$age,factor(u$treat),p.adjust.method='bonferroni',pool.sd=TRUE)$p.value
ij<-which(!is.na(pw),arr.ind=TRUE)
write_out(data.frame(treatment1=paste0(as.integer(rownames(pw)[ij[,1]])+1,'Alt'),
 treatment2=paste0(as.integer(colnames(pw)[ij[,2]])+1,'Alt'),p_bonferroni=pw[ij]),'table2_age_pairwise_tests.csv')
# Table 3 denominator: all eight retained choice occasions per respondent.
c3<-table(factor(d$treat,levels=1:5),factor(d$choibio,levels=1:6))
p3<-100*prop.table(c3,1)
t3<-data.frame(Treatment=paste0(labels,': SQ + ',1:5),check.names=FALSE)
for(j in 1:6) t3[[c('Status quo','Program A','Program B','Program C','Program D','Program E')[j]]]<-
 ifelse(j<=2:6,sprintf('%.1f',p3[,j]),'\u2014')
write_out(t3,'table3_display.csv')
write_out(data.frame(treatment=rep(labels,each=6),alternative=rep(c('SQ',LETTERS[1:5]),5),
 available=rep(1:6,5)<=rep(2:6,each=6),count=as.vector(t(c3)),
 denominator=rep(8*N,each=6),percent=as.vector(t(p3))),'table3_statistics.csv')
# Table 4 denominator: respondents; Total is pooled, not an average of percentages.
c4<-table(factor(u$sq_choices,levels=0:8),factor(u$treat,levels=1:5))
p4<-100*prop.table(c4,2)
t4<-data.frame(`SQ choices`=c('0 (never)',as.character(1:7),'8 (always)'),check.names=FALSE)
for(tr in 1:5) t4[[labels[tr]]]<-sprintf('%.1f',p4[,tr])
t4$Total<-sprintf('%.1f',100*rowSums(c4)/nrow(u))
write_out(t4,'table4_display.csv')
write_out(data.frame(sq_choices=rep(0:8,6),treatment=rep(c(labels,'Total'),each=9),
 count=c(as.vector(c4),rowSums(c4)),denominator=rep(c(N,sum(N)),each=9),
 percent=c(as.vector(p4),100*rowSums(c4)/sum(N))),'table4_statistics.csv')
stopifnot(all(rowSums(c3)==8*N),all(colSums(c4)==N),
 all(colSums(c4*(0:8))==c3[,1]),abs(sum(p3[1,])-100)<1e-10)
raw_u<-raw[!duplicated(raw$id),]
audit<-data.frame(treatment=labels,original_respondents=as.integer(table(raw_u$treat)),
 excluded_education6=as.integer(table(factor(raw_u$treat[raw_u$v375==6],levels=1:5))),
 retained_respondents=N,retained_choices=8*N,
 gender_code0_retained=as.integer(table(factor(u$treat[u$v372==0],levels=1:5))))
write_out(audit,'sample_audit.csv')
writeLines(c('Source data MD5:',unname(tools::md5sum(file.path(github_dir,'PrefMatWideFormatFullBiogeme.txt'))),
 capture.output(sessionInfo())),file.path(out_dir,'session_info.txt'))
print(audit,row.names=FALSE); print(tests,row.names=FALSE)
cat('\nTables 2--4 written to',normalizePath(out_dir),'\n')
