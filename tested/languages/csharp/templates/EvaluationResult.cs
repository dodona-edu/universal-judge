using System;
using System.Collections;

namespace Tested {
  public record Message(string Description, string Format = "text", string? Permission = null);
  public record EvaluationResult(bool Result, string? ReadableExpected = null, string? ReadableActual = null, List<Message>? Messages = null)
  {
    public List<Message> Messages { get; init; } = Messages ?? new List<Message>();
  }
}
