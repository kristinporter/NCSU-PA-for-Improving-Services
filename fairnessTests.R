#-------------------------#
learnersSelect   <- learnersSelect
equityVarsSelect <- equityVarsSelect
thresholdsSelect <- 0.3
#-------------------------#

equityTests <- calc_equity_tests_all(
  learnersSelect,
  equityVarsSelect,
  thresholdsSelect
)

print(round_table(equityTests))
