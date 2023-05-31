using System;
using System.IO;
using System.Text.Json;
using System.Collections;
using System.Collections.Generic;
using System.Numerics;
using System.Linq;
using System.Runtime.CompilerServices;
using Tested;

namespace Tested
{
    class Values
    {
        private static List<Dictionary<string, object?>> encodeSequence(IEnumerable objects) {
            var results = new List<Dictionary<string, object?>>();
            foreach (var obj in objects) {
                results.Add(internalEncode(obj));
            }
            return results;
        }

        private static Dictionary<string, object?> internalEncode(Object? value) {
            string type;
            object? data = value;
            string? diagnostic = null;

            if (value == null) {
                type = "nothing";
            } else if (value is Boolean) {
                type = "boolean";
            } else if (value is Array) {
                type = "array";
                data = encodeSequence((IEnumerable) value);
            } else if (value is ITuple) {
                type = "tuple";
                var r = new List<Object?>();
                var t = (ITuple) value;
                for (var n = 0; n < t.Length; n++) {
                  r.Add(t[n]);
                }
                data = encodeSequence(r);
            } else if (value is BigInteger) {
                type = "bigint";
                data = value.ToString();
            } else if (value is Byte) {
                type = "uint8";
            } else if (value is SByte) {
                type = "int8";
            } else if (value is Int16) {
                type = "int16";
            } else if (value is UInt16) {
                type = "uint16";
            } else if (value is Int32) {
                type = "int32";
            } else if (value is UInt32) {
                type = "uint32";
            } else if (value is Int64) {
                type = "int64";
            } else if (value is UInt64) {
                type = "uint64";
            } else if (value is Single) {
                type = "single_precision";
                var v = (Single) value;
                if (Single.IsNaN(v)) {
                    data = "nan";
                } else if (Single.IsNegativeInfinity(v)) {
                    data = "-inf";
                } else if (Single.IsPositiveInfinity(v)) {
                    data = "inf";
                }
            } else if (value is Double) {
                type = "double_precision";
                var v = (Double) value;
                if (Double.IsNaN(v)) {
                    data = "nan";
                } else if (Double.IsNegativeInfinity(v)) {
                    data = "-inf";
                } else if (Double.IsPositiveInfinity(v)) {
                    data = "inf";
                }
            } else if (value is string) {
                type = "text";
            } else if (value is IList) {
                type = "list";
                data = encodeSequence((IEnumerable) value);
            } else if (value is ISet<object>) {
                type = "set";
                data = encodeSequence((IEnumerable) value);
            } else if (value is IDictionary) {
                type = "map";
                List<DictionaryEntry> entries = new List<DictionaryEntry>();
                foreach (DictionaryEntry entry in (IDictionary) value)
                {
                  entries.Add(new DictionaryEntry {Key = internalEncode(entry.Key), Value = internalEncode(entry.Value)});
                }
                data = entries;
            } else {
                type = "unknown";
                diagnostic = data.GetType().ToString();
                data = JsonSerializer.Serialize(data);
            }
            var result = new Dictionary<string, object?>();
            result.Add("type", type);
            result.Add("data", data);
            result.Add("diagnostic", diagnostic);
            return result;
        }

        public static void WriteValue(StreamWriter writer, Object? value)
        {
          var serializeOptions = new JsonSerializerOptions
          {
              PropertyNamingPolicy = JsonNamingPolicy.CamelCase
          };
          var result = internalEncode(value);
          string stringResult = JsonSerializer.Serialize(result, serializeOptions);
          writer.Write(stringResult);
        }

        public static void WriteException(StreamWriter writer, Exception? e)
        {
          if (e == null) {
            return;
          }

          Dictionary<string, string> data = new Dictionary<string, string>();
          data.Add("message", e.Message ?? "");
          data.Add("stacktrace", e.StackTrace ?? "");
          data.Add("type", e.GetType().Name);

          string stringResult = JsonSerializer.Serialize(data);
          writer.Write(stringResult);
        }

        public static void Evaluated(StreamWriter writer, bool result, string expected, string actual, List<Message> messages)
        {
          var serializeOptions = new JsonSerializerOptions
          {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
          };
          SendEvaluated(writer, new EvaluationResult(result, expected, actual, messages));
        }

        public static void SendEvaluated(StreamWriter writer, EvaluationResult result)
        {
          var serializeOptions = new JsonSerializerOptions
          {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
          };
          var data = new Dictionary<string, string>();

          string stringResult = JsonSerializer.Serialize(result, serializeOptions);
          writer.Write(stringResult);
        }
    }
}
