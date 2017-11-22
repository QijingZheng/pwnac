#!/bin/bash
################################################################################

# Suppose we have NSW = 2000 structures extracted from a MD trajectory. For each
# structure, an 'scf' calculation is performed, i.e. we have 2000 'scf'
# calculations in total. The directories where the 'scf' calculations are
# arranged as:
#  0001/, 0002/, ... , 2000/
NSW=2000
# number of digits of NSW, this determines the number of leading zeros of each
# directories.
NDIGIT=$(echo -n ${NSW} | wc -c)
export DIRFORM='%0'${NDIGIT}'d'

# number of bands in each 'scf' calculation. Make sure that this number is the
# same for all the 'scf' calcultions.
NBANDS=108
# "BMIN" and "BMAX" determine the basis range of NAMD calculatons.
BMIN=1
BMAX=108
# number of processes in xargs command, which should be less than number of
# CPUs.
NPROC=4
# path of pwnac, a program used to calculate the NAC between two wavefunctions.
export PWNAC=./pwnac

################################################################################
function nacFunc ()
{
    # program used to compute NAC between two wavefunctions
    local n=${1}
    local dir1=$(printf "${DIRFORM}" ${n})
    local dir2=$(printf "${DIRFORM}" $((n+1)))

    echo ${dir1} ${dir2}
    if [ ! -f ${dir1}/pwscf.save/K00001/nac.dat ]; then
        ${PWNAC} ${dir1}/pwscf.save/K0001 ${dir2}/pwscf.save/K00001
    fi
}
export -f nacFunc

# instead of using a for loop, we utilize the xargs command which can
# parallelize the calculations

# for ii in `seq 1 $((NSW-1))`
# do
#     jj=$((ii+1))
#     dir1=$(printf "${DIRFORM}" ${ii})
#     dir2=$(printf "${DIRFORM}" ${jj})
#
#     ${PWNAC} ${dir1}/pwscf.save/K0001 ${dir2}/pwscf.save/K00001
# done

eval "echo -e {1..$((NSW-1))}'\n' " | xargs -I {} -P ${NPROC} bash -c "nacFunc {}"
################################################################################

python << EOF
nband   = ${NBANDS}
bmin    = ${BMIN}
bmax    = ${BMAX}
nsw     = ${NSW}
ndigit  = len('%s' % nsw)
dirform = "%%0%dd" % ndigit

# print nband, bmin, bmax, nsw, ndigit, dirform

# print '\n'.join([dirform % ii + "/pwscf.save/K00001/eig.dat"
#                 for ii in range(1, nsw)])
# print '\n'.join([dirform % ii + "/pwscf.save/K00001/nac.dat"
#                 for ii in range(1, nsw)])

eigs  = np.array([np.loadtxt(dirform % ii + "/pwscf.save/K00001/eig.dat")
                for ii in range(1, nsw)])
nacs  = np.array([np.loadtxt(dirform % ii + "/pwscf.save/K00001/nac.dat")
                for ii in range(1, nsw)]).reshape((nsw-1, nband, nband))

np.savetxt('EIGTXT', eigs(:,bmin-1:bmax))
np.savetxt('NATXT',  nacs(:,bmin-1:bmax, bmin-1:bmax))
EOF
