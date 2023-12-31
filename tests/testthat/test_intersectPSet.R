library(PharmacoGx)
require(parallel)
context("Checking intersectPSet.")

# ###TO-DO:: This test takes forever to run; consider making the intersections smaller
# test_that("Intersection result did not change since last time", {
# 	data(GDSCsmall)
# 	data(CCLEsmall)
# 	common <- intersectPSet(list('GDSC'=GDSCsmall, 'CCLE'=CCLEsmall), intersectOn = c("drugs", "cell.lines","concentrations"))
# 	expect_equal_to_reference(common, "intersectedSmallData.rds", tolerance=1e-3)
# 	expect_equal(sum(!is.na(common$sensitivityProfiles(CCLE)$auc_recomputed_star)),sum(!is.na(common$sensitivityProfiles(CCLE)$auc_recomputed_star)))
# 	expect_equal(treatmentNames(common$CCLE), treatmentNames(common$GDSC))
# 	expect_equal(sampleNames(common$CCLE), sampleNames(common$GDSC))
# })
#
# test_that("Strict Intersection result did not change since last time", {
# 	data(GDSCsmall)
# 	data(CCLEsmall)
# 	common <- intersectPSet(list('GDSC'=GDSCsmall, 'CCLE'=CCLEsmall), intersectOn = c("drugs", "cell.lines","concentrations"), strictIntersect=TRUE)
# 	expect_equal_to_reference(common, "intersectedSmallDataStrict.rds", tolerance=1e-3)
# 	expect_equal(sum(!is.na(common$sensitivityProfiles(CCLE)$auc_recomputed_star)),sum(!is.na(common$sensitivityProfiles(CCLE)$auc_recomputed_star)))
# 	expect_equal(treatmentNames(common$CCLE), treatmentNames(common$GDSC))
# 	expect_equal(sampleNames(common$CCLE), sampleNames(common$GDSC))
# 	expect_equal(rownames(sensitivityProfiles(common$GDSC)), rownames(common$sensitivityProfiles(CCLE)))
# })
