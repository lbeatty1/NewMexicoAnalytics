# NewMexicoAnalytics
Lauren Beatty
lbeatty@edf.org
Environmental Defense Fund

AnalyzeNewMexico.R takes publically available data on well statuses and well production to calculate the FA bonds and liabilities for operators in New Mexico.  It also produces a panel of well API-month status and production so you can see how wells flow between different statuses, and how production changes over time.  Lastly, it outputs a series of figures intended to compare bonding amounts to plugging liabilities.

AnalyzeUtahProposedReg.R takes New Mexico data and calculated hypothetical bond amounts under the new Utah proposed regulation.  It calls some simple helper functions in Utah_bond_funs.R

## Data Access
These scripts use two datasources.  The first is the wellhistory data accessable from EMNRD OCD's FTP server.  I converted this xml file to a csv before working with it.  I accessed production data from GO-TECH, available at http://octane.nmt.edu/gotech/Petroleum_Data/county.aspx


