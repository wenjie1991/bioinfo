#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(argparser))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))

p = arg_parser("Calculate the coverage on target by region name")
p = add_argument(p, "input_bam", help = "bam file contain the aligned resds", nargs = 1)
p = add_argument(p, "input_target", help = "bed file contain the target region with name in the 4th columne ")
p = add_argument(p, "--output", help = "bed file contain the target region with name in the 4th columne ", default = "./output.tsv")
p = add_argument(p, "--keep", help = "keep the template file", flag = T, default = FALSE)
p = add_argument(p, "--tmp", help = "template fold", default = "./")
pa = parse_args(p)

bam_file = ifelse(
    (grepl("\\.bam$", pa$input_bam) & file.exists(pa$input_bam)), 
    pa$input_bam,
    stop("Input bam not accessable or with wrong appendix.", call. = F)
    )
input_target = ifelse(
    (grepl("\\.bed$", pa$input_target) & file.exists(pa$input_target)),
    pa$input_target,
    stop("Target file error.", call. = F)
    )
tmp = ifelse(
    dir.exists(pa$tmp), 
    pa$tmp,
    {dir.create(pa$tmp, recur = T, show = F); pa$tmp}
    )
output = ifelse(
    dir.exists(dirname(pa$output)),
    pa$output,
    {dir.create(dirname(pa$output), recur = T, show = F); pa$outupt}
    )


sample_mark = sub("\\.bam", "", basename(bam_file))
coverage_bed = paste0(pa$tmp, sample_mark, "_coverage.bed")

cmd = sprintf("bedtools coverage -b %s -a %s > %s -d", bam_file, input_target, coverage_bed)
write("[Get base coverage]", stderr())
write(cmd, stderr())
system(cmd)

write("[Calculate category based depth]", stderr())
x = fread(coverage_bed)
coverage_summary = x[,
    .(
        coverage_10X = sum(V6 > 10) / .N,
        coverage_20X = sum(V6 > 20) / .N,
        coverage_50X = sum(V6 > 50) / .N,
        coverage_100X = sum(V6 > 100) / .N,
        coverage_200X = sum(V6 > 200) / .N,
        region_size = sum(abs(V3 - V2))
        )
    , by = V4][, .(Gene = V4, coverage_10X, coverage_20X, coverage_50X, coverage_100X, coverage_200X, region_size)]
write_tsv(coverage_summary, output)
write("[Output in]", stderr())
write(output, stderr())

if (!pa$keep) {
    invisible(file.remove(coverage_bed))
}

