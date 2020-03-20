"""
Code for contexts.
"""
from dataclasses import field
from typing import List, Optional, Union

from pydantic import root_validator
from pydantic.dataclasses import dataclass

from features import FeatureSet, combine_features, WithFeatures
from testplan.channels import TextData
from testplan.testcase import ContextTestcase, Testcase

Code = Union[str, TextData]


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
