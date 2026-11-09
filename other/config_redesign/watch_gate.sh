#!/bin/bash
# Emit one line per gate scenario once every taxsim SLURM job has drained, then
# exit. Written for the Phase 4 gate; the vintage tags are its only assumption.
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator
V=/nfs/roberts/scratch/pi_nrs36/jar335/model_data/Tax-Simulator/v1
while true; do
  n=$(squeue -u jar335 -h --format="%j" 2>/dev/null | grep -c '^taxsim')
  if [ "${n:-0}" -eq 0 ]; then
    for tag in s2 s3 s4 s6 s7; do
      g=${tag#s}
      out=$(bash other/config_redesign/gate_diff.sh "$V/rb_p5b_${tag}" "$V/golds${g}" 2>&1 | tail -1)
      case "$out" in
        GATE_PASS*) echo "GATE $tag PASS" ;;
        *)          echo "GATE $tag FAIL: $(echo "$out" | cut -c1-160)" ;;
      esac
    done
    echo "GATE all five scenarios compared"
    exit 0
  fi
  sleep 60
done
