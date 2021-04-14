#!/usr/bin/env bash
<%! from json import dumps %>\

function write_context_separator {
    echo -n "--${context_secret_id}-- SEP" >${value_file}
    echo -n "--${context_secret_id}-- SEP" >${exception_file}
    echo -n "--${context_secret_id}-- SEP"
    echo -n "--${context_secret_id}-- SEP" >&2
}

function write_separator {
    echo -n "--${secret_id}-- SEP" >${value_file}
    echo -n "--${secret_id}-- SEP" >${exception_file}
    echo -n "--${secret_id}-- SEP"
    echo -n "--${secret_id}-- SEP" >&2
}

write_context_separator
source ./${submission_name}.sh \
% for argument in run_testcase.arguments:
    ${dumps(value.data)} \
% endfor
