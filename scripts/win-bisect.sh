#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  scripts/win-bisect.sh [plan|list|left|right] [options]

Purpose:
  Help bisect Windows-only crashes that only reproduce in package-mode
  tinytest runs. The script splits the ordered test list into halves,
  appends a detector tail, and either prints or runs the corresponding
  `scripts/win-test.sh pkgfiles ...` command.

Modes:
  plan   Print the current suspect range and the next left/right commands
  list   Print the ordered test list with 1-based indices
  left   Run the left half of the current suspect range plus the detector tail
  right  Run the right half of the current suspect range plus the detector tail

Options:
  --start N        1-based start index in the suspect prefix
  --end N          1-based end index in the suspect prefix
  --runner MODE    Test runner mode for win-test.sh (default: pkgfiles)
  --trace          Prefix run commands with RTINYCC_TRACE_FINALIZERS=1
  --tail FILE      Append a detector-tail file; repeatable.
                   Default:
                     inst/tinytest/test_unions.R
                     inst/tinytest/test_variadic.R

Examples:
  scripts/win-bisect.sh plan
  scripts/win-bisect.sh left
  scripts/win-bisect.sh right
  scripts/win-bisect.sh plan --start 1 --end 11
  scripts/win-bisect.sh left --trace
  scripts/win-bisect.sh plan --tail inst/tinytest/test_unions.R
EOF
}

mode="${1:-plan}"
case "$mode" in
  -h|--help)
    usage
    exit 0
    ;;
  plan|list|left|right)
    shift || true
    ;;
  *)
    echo "unknown mode: $mode" >&2
    usage >&2
    exit 1
    ;;
esac

runner="pkgfiles"
trace=0
tail_files=()
start=""
end=""

while [ $# -gt 0 ]; do
  case "$1" in
    --start)
      start="${2:-}"
      shift 2
      ;;
    --end)
      end="${2:-}"
      shift 2
      ;;
    --runner)
      runner="${2:-}"
      shift 2
      ;;
    --trace)
      trace=1
      shift
      ;;
    --tail)
      tail_files+=("${2:-}")
      shift 2
      ;;
    *)
      echo "unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [ "${#tail_files[@]}" -eq 0 ]; then
  tail_files=(
    "inst/tinytest/test_unions.R"
    "inst/tinytest/test_variadic.R"
  )
fi

mapfile -t all_tests < <(
  cd "$repo_root"
  printf '%s\n' inst/tinytest/test_*.R | sort
)

if [ "${#all_tests[@]}" -eq 0 ]; then
  echo "no test files found under inst/tinytest" >&2
  exit 1
fi

tail_lookup="|"
for f in "${tail_files[@]}"; do
  tail_lookup+="$f|"
done

prefix_tests=()
tail_start_index=0
for i in "${!all_tests[@]}"; do
  f="${all_tests[$i]}"
  if [[ "$tail_lookup" == *"|$f|"* ]]; then
    tail_start_index=$((i + 1))
    break
  fi
  prefix_tests+=("$f")
done

if [ "$tail_start_index" -eq 0 ]; then
  echo "tail files do not appear in the ordered test list:" >&2
  printf '  %s\n' "${tail_files[@]}" >&2
  exit 1
fi

prefix_count="${#prefix_tests[@]}"
if [ "$prefix_count" -eq 0 ]; then
  echo "no prefix tests remain before the detector tail" >&2
  exit 1
fi

if [ -z "$start" ]; then
  start=1
fi
if [ -z "$end" ]; then
  end="$prefix_count"
fi

if ! [[ "$start" =~ ^[0-9]+$ && "$end" =~ ^[0-9]+$ ]]; then
  echo "--start and --end must be positive integers" >&2
  exit 1
fi

if [ "$start" -lt 1 ] || [ "$end" -lt "$start" ] || [ "$end" -gt "$prefix_count" ]; then
  echo "invalid suspect range: start=$start end=$end prefix_count=$prefix_count" >&2
  exit 1
fi

if [ "$mode" = "list" ]; then
  for i in "${!all_tests[@]}"; do
    printf '%2d  %s\n' "$((i + 1))" "${all_tests[$i]}"
  done
  exit 0
fi

range_size=$((end - start + 1))
mid=$(((start + end) / 2))

left_start="$start"
left_end="$mid"
right_start=$((mid + 1))
right_end="$end"

build_subset() {
  local subset_start="$1"
  local subset_end="$2"
  local out=()
  local idx
  if [ "$subset_start" -gt "$subset_end" ]; then
    printf '%s\n' ""
    return
  fi
  for ((idx = subset_start; idx <= subset_end; idx++)); do
    out+=("${prefix_tests[$((idx - 1))]}")
  done
  printf '%s\n' "${out[@]}"
}

mapfile -t left_tests < <(build_subset "$left_start" "$left_end")
mapfile -t right_tests < <(build_subset "$right_start" "$right_end")

run_subset() {
  local subset_name="$1"
  shift
  local subset=("$@")
  local cmd=("$script_dir/win-test.sh" "$runner")
  cmd+=("${subset[@]}")
  cmd+=("${tail_files[@]}")
  if [ "$trace" -eq 1 ]; then
    RTINYCC_TRACE_FINALIZERS=1 "${cmd[@]}"
  else
    "${cmd[@]}"
  fi
}

print_cmd() {
  local subset_name="$1"
  local subset_start="$2"
  local subset_end="$3"
  local trace_prefix=""
  if [ "$trace" -eq 1 ]; then
    trace_prefix="RTINYCC_TRACE_FINALIZERS=1 "
  fi
  printf '%s%s %s --start %d --end %d' \
    "$trace_prefix" \
    "scripts/win-bisect.sh" \
    "$subset_name" \
    "$subset_start" \
    "$subset_end"
  if [ "$runner" != "pkgfiles" ]; then
    printf ' --runner %s' "$runner"
  fi
  if [ "$trace" -eq 1 ]; then
    printf ' --trace'
  fi
  local tf
  local using_default_tail=1
  if [ "${#tail_files[@]}" -ne 2 ] ||
    [ "${tail_files[0]}" != "inst/tinytest/test_unions.R" ] ||
    [ "${tail_files[1]}" != "inst/tinytest/test_variadic.R" ]; then
    using_default_tail=0
  fi
  if [ "$using_default_tail" -eq 0 ]; then
    for tf in "${tail_files[@]}"; do
      printf ' --tail %s' "$tf"
    done
  fi
  printf '\n'
}

if [ "$mode" = "left" ]; then
  run_subset left "${left_tests[@]}"
  exit 0
fi

if [ "$mode" = "right" ]; then
  if [ "$right_start" -gt "$right_end" ]; then
    echo "right half is empty for range $start..$end" >&2
    exit 1
  fi
  run_subset right "${right_tests[@]}"
  exit 0
fi

echo "Ordered test list before detector tail: $prefix_count files"
echo "Detector tail:"
printf '  %s\n' "${tail_files[@]}"
echo
echo "Current suspect range: $start..$end ($range_size files)"
for ((idx = start; idx <= end; idx++)); do
  printf '  %2d  %s\n' "$idx" "${prefix_tests[$((idx - 1))]}"
done
echo
echo "Left half:  $left_start..$left_end"
if [ "$right_start" -le "$right_end" ]; then
  echo "Right half: $right_start..$right_end"
else
  echo "Right half: <empty>"
fi
echo
echo "Next commands:"
print_cmd left "$left_start" "$left_end"
if [ "$right_start" -le "$right_end" ]; then
  print_cmd right "$right_start" "$right_end"
fi
