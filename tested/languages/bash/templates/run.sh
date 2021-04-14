#!/usr/bin/env bash
<%! from json import dumps %>\

echo -n "--${context_secret_id}-- SEP" >${value_file}
echo -n "--${context_secret_id}-- SEP" >${exception_file}
echo -n "--${context_secret_id}-- SEP"
echo -n "--${context_secret_id}-- SEP" >&2

./${submission_name}.sh \
% for argument in run_testcase.arguments:
    ${dumps(value.data)} \
% endfor
