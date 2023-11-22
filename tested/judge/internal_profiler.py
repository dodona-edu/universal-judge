from time import monotonic

from attr import define, field

from tested.dodona import ExtendedMessage, Permission


@define
class Measurement:
    name: str
    start: float
    end: float | None = None
    nested: list["Measurement"] = field(factory=list)

    def to_message(self, default_open: bool = False) -> str:
        assert self.end
        if self.nested:
            nested_result = ""
            for n in self.nested:
                nested_result += n.to_message()
                nested_result += "\n"
            return f"""
                    <details {"open" if default_open else ""}>
                    <summary>{self.name}: {self.end - self.start:.2f}s</summary>
                    <p>{nested_result}</p>
                    </details>
                    """
        else:
            return f"{self.name}: {self.end - self.start:.2f}s<br>"


class Profiler:
    root_measurement: Measurement
    currently_open: list[Measurement]

    def __init__(self):
        self.root_measurement = Measurement(name="Execution", start=monotonic())
        self.currently_open = [self.root_measurement]

    def start(self, name: str):
        measurement = Measurement(name=name, start=monotonic())
        self.currently_open[-1].nested.append(measurement)
        self.currently_open.append(measurement)

    def stop(self, name: str):
        assert (
            self.currently_open[-1].name == name
        ), f"Expected {self.currently_open[-1].name}, got {name}"
        m = self.currently_open.pop()
        m.end = monotonic()

    def to_message(self) -> ExtendedMessage:
        if not self.root_measurement.end:
            self.root_measurement.end = monotonic()
        return ExtendedMessage(
            description=f"""
            <h2>Time measurements</h2>
            Note that enabling profiling disables parallel execution, if that was enabled.<br>
            These measurements can be used to check which part of TESTed is using a lot of time.<br>
            Measurements are rounded to .2f, so they might not add up.<br>
            Not everything is measured: expect Dodona-reported execution to be longer.
            <style>
            details > *:not(summary) {{
                margin-left: 2em;
            }}
            </style>
            <p>{self.root_measurement.to_message(True)}</p>
            """,
            format="html",
            permission=Permission.STAFF,
        )


class DummyProfiler:
    def start(self, name: str):
        pass

    def stop(self, name: str):
        pass
