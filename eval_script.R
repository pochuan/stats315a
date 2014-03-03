# Evaluation script
#
# Please make sure your submission doesn't generate any errors
# with this script before uploading it to Coursework.

rm(list = ls())

# For now, test that your submission works with random test labels.
n.test = 1497
test.labels = rbinom(n.test, 1, 0.5)

# Read in the predictions file.
# You may need to change the file path here.
z = read.table("z.txt")

# This is your error rate. Hopefully this number will
# get smaller when we use non-random test labels!
mean(z != test.labels)