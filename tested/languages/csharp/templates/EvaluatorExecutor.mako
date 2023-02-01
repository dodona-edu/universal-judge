## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,function" />\

using System;
using System.IO;
using Tested;

namespace Tested
{
    public class EvaluatorExecutor
    {
        public static void Main(string[] args)
        {
            var result = <%include file="function.mako" args="function=function" />;
            StreamWriter writer = new StreamWriter(Console.OpenStandardOutput());
            writer.AutoFlush = true;
            Console.SetOut(writer);
            Values.SendEvaluated(writer, result);
        }
    }
}
