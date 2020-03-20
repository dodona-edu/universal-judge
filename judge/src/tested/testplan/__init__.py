"""
Data format for the testplan.

This module is the authoritative source on the format and behaviour of the testplan.
When executing this module, a json-schema is generated for the format, which can be
of assistance when checking existing testplans.
"""
import json
from dataclasses import field
from enum import Enum
from pathlib import Path
from typing import List, Optional, Dict, Any

from pydantic import BaseModel, root_validator
from pydantic.dataclasses import dataclass

from features import FeatureSet, combine_features, WithFeatures
from testplan import ContextTestcase, TextData
from testplan.channels import TextData
from testplan.testcase import ContextTestcase, Testcase


Code = Dict[str, TextData]


@dataclass
class Context(WithFeatures):
    """
    A test case is an independent run of the solution.
    """
    context_testcase: ContextTestcase = ContextTestcase()
    testcases: List[Testcase] = field(default_factory=list)
    before: Code = field(default_factory=dict)
    after: Code = field(default_factory=dict)
    description: Optional[str] = None

    @root_validator
    def check_testcases_exist(cls, values):
        context: ContextTestcase = values.get('context_testcase')
        additional = values.get('testcases')
        if not context.input.main_call and not additional:
            raise ValueError("A context needs a context testcase or at least one "
                             "normal testcase.")
        return values

    def get_used_features(self) -> FeatureSet:
        return combine_features([
            self.context_testcase.get_used_features(),
            combine_features([x.get_used_features() for x in self.testcases])
        ])

    def get_stdin(self, resources: Path):
        return self.context_testcase.input.get_as_string(resources)


@dataclass
class Tab(WithFeatures):
    """Represents a tab on Dodona."""
    name: str
    contexts: List[Context]

    def get_used_features(self) -> FeatureSet:
        return combine_features(x.get_used_features() for x in self.contexts)


class ExecutionMode(str, Enum):
    PRECOMPILATION = "precompilation"
    INDIVIDUAL = "individual"


@dataclass
class Configuration:
    """
    The configuration options for the judge.
    """
    parallel: bool = True
    """
    Indicate that the contexts should be executed in parallel. It is recommended to
    disable this for exercises that already are multithreaded. It may also be worth
    investigating if the exercise is computationally heady.
    """
    mode: ExecutionMode = ExecutionMode.PRECOMPILATION
    """
    The default mode for the judge.
    """
    allow_fallback: Optional[bool] = True
    """
    Indicate if the judge should attempt individual mode if the precompilation mode
    fails. If nothing is given, the language-dependent default is used. If a boolean
    is given, this value is used, regardless of the language default.
    """
    language: Dict[str, Dict[str, Any]] = field(default_factory=dict)
    """
    Language-specific options for the judge. These depend on the language
    implementation; the judge itself does nothing with it.
    """


@dataclass
class Plan(WithFeatures):
    """General test plan, which is used to run tests of some code."""
    tabs: List[Tab] = field(default_factory=list)
    namespace: str = "Main"
    configuration: Configuration = Configuration()

    def get_used_features(self) -> FeatureSet:
        return combine_features(x.get_used_features() for x in self.tabs)

    def config_for(self, language: str) -> dict:
        return self.configuration.language.get(language, dict())


class _PlanModel(BaseModel):
    __root__: Plan


def parse_test_plan(json_string) -> Plan:
    """Parse a test plan into the structures."""
    return _PlanModel.parse_raw(json_string).__root__


def generate_schema():
    """
    Generate a json schema for the serialisation type. It will be printed on stdout.
    """
    sc = _PlanModel.schema()
    sc['$schema'] = "http://json-schema.org/schema#"
    sc['title'] = "Testplan"
    print(json.dumps(sc, indent=2))


if __name__ == '__main__':
    # with open('../exercise/zoemzinnen/preparation/plan.json', 'r') as f:
    #     r = parse_test_plan(f.read())
    #     print(r)
    generate_schema()
