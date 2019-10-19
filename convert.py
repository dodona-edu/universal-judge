#!/usr/bin/env python
import argparse

from testplan import Test, Input, Output, ChannelData, Testcase, Context, Tab, Plan


def generate_groovy(inputs, outputs, result):
    with open(result, 'w') as file:

        file.write("plano {\n")
        file.write("\ttab{\n")
        file.write("\t\tname 'Correctheid'\n")
        for input_, output_ in zip(inputs, outputs):
            file.write("\t\ttest {\n")
            file.write(f"\t\t\tinput '{input_}'\n")
            file.write(f"\t\t\toutput '{output_}'\n")
            file.write("\t\t}\n")

        file.write("\t}\n")
        file.write("}")


def convert(input_file, output_file, result, groovy):
    """
    Convert in and out files to the new format.
    """

    with open(input_file, 'r') as file:
        inputs = file.read().splitlines(keepends=False)
    with open(output_file, 'r') as file:
        outputs = file.read().splitlines(keepends=False)

    if groovy:
        print("Generating groovy...")
        generate_groovy(inputs, outputs, result)
        return

    # Construct contexts.
    contexts = []
    for input_, output_ in zip(inputs, outputs):
        stdin_ = Input(stdin=ChannelData(data=input_))
        stdout_ = Output(stdout=ChannelData(data=output_))
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
    parser.add_argument("-d", action='store_true')
    parser.add_argument('inp')
    parser.add_argument('out')
    parser.add_argument('result')
    args = parser.parse_args()
    convert(args.inp, args.out, args.result, args.d)
