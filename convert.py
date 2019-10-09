#!/usr/bin/env python
import argparse

from testplan import Test, Input, Stdin, Output, Stdout, Testcase, Context, Tab, Plan


def convert(input_file, output_file, result):
    """
    Convert in and out files to the new format.
    """

    with open(input_file, 'r') as file:
        inputs = file.read().splitlines(keepends=False)
    with open(output_file, 'r') as file:
        outputs = file.read().splitlines(keepends=False)

    # Construct contexts.
    contexts = []
    for input_, output_ in zip(inputs, outputs):
        stdin_ = Input(stdin=Stdin(data=input_))
        stdout_ = Output(stdout=Stdout(data=output_))
        test = Test(description=input_, input=stdin_, output=stdout_)
        testcase = Testcase(description=input_, tests=[test])
        contexts.append(Context(testcases=[testcase]))

    tab = Tab(name="Correctheid", contexts=contexts)
    plan = Plan(tabs=[tab])

    result_string = plan.to_json()
    with open(result, 'w') as file:
        file.write(result_string)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Convert in/out files to new format')
    parser.add_argument('inp')
    parser.add_argument('out')
    parser.add_argument('result')
    args = parser.parse_args()
    convert(args.inp, args.out, args.result)
